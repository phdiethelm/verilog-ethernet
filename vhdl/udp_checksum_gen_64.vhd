-- 
-- Copyright (c) 2016-2018 Alex Forencich
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
-- 

-- Language: VHDL 2008

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.vhdl_pkg.all;
--
-- UDP checksum calculation module (64 bit datapath)
--
entity udp_checksum_gen_64 is
    generic (
        PAYLOAD_FIFO_DEPTH : integer := 2048;
        HEADER_FIFO_DEPTH  : integer := 8
    );
    port (
        signal clk                       : in  std_logic;
        signal rst                       : in  std_logic;

        --
        -- UDP frame input
        --
        signal s_udp_hdr_valid           : in  std_logic;
        signal s_udp_hdr_ready           : out std_logic;
        signal s_eth_dest_mac            : in  std_logic_vector(47 downto 0);
        signal s_eth_src_mac             : in  std_logic_vector(47 downto 0);
        signal s_eth_type                : in  std_logic_vector(15 downto 0);
        signal s_ip_version              : in  std_logic_vector(3 downto 0);
        signal s_ip_ihl                  : in  std_logic_vector(3 downto 0);
        signal s_ip_dscp                 : in  std_logic_vector(5 downto 0);
        signal s_ip_ecn                  : in  std_logic_vector(1 downto 0);
        signal s_ip_identification       : in  std_logic_vector(15 downto 0);
        signal s_ip_flags                : in  std_logic_vector(2 downto 0);
        signal s_ip_fragment_offset      : in  std_logic_vector(12 downto 0);
        signal s_ip_ttl                  : in  std_logic_vector(7 downto 0);
        signal s_ip_header_checksum      : in  std_logic_vector(15 downto 0);
        signal s_ip_source_ip            : in  std_logic_vector(31 downto 0);
        signal s_ip_dest_ip              : in  std_logic_vector(31 downto 0);
        signal s_udp_source_port         : in  std_logic_vector(15 downto 0);
        signal s_udp_dest_port           : in  std_logic_vector(15 downto 0);
        signal s_udp_payload_axis_tdata  : in  std_logic_vector(63 downto 0);
        signal s_udp_payload_axis_tkeep  : in  std_logic_vector(7 downto 0);
        signal s_udp_payload_axis_tvalid : in  std_logic;
        signal s_udp_payload_axis_tready : out std_logic;
        signal s_udp_payload_axis_tlast  : in  std_logic;
        signal s_udp_payload_axis_tuser  : in  std_logic;

        --
        -- UDP frame output
        --
        signal m_udp_hdr_valid           : out std_logic;
        signal m_udp_hdr_ready           : in  std_logic;
        signal m_eth_dest_mac            : out std_logic_vector(47 downto 0);
        signal m_eth_src_mac             : out std_logic_vector(47 downto 0);
        signal m_eth_type                : out std_logic_vector(15 downto 0);
        signal m_ip_version              : out std_logic_vector(3 downto 0);
        signal m_ip_ihl                  : out std_logic_vector(3 downto 0);
        signal m_ip_dscp                 : out std_logic_vector(5 downto 0);
        signal m_ip_ecn                  : out std_logic_vector(1 downto 0);
        signal m_ip_length               : out std_logic_vector(15 downto 0);
        signal m_ip_identification       : out std_logic_vector(15 downto 0);
        signal m_ip_flags                : out std_logic_vector(2 downto 0);
        signal m_ip_fragment_offset      : out std_logic_vector(12 downto 0);
        signal m_ip_ttl                  : out std_logic_vector(7 downto 0);
        signal m_ip_protocol             : out std_logic_vector(7 downto 0);
        signal m_ip_header_checksum      : out std_logic_vector(15 downto 0);
        signal m_ip_source_ip            : out std_logic_vector(31 downto 0);
        signal m_ip_dest_ip              : out std_logic_vector(31 downto 0);
        signal m_udp_source_port         : out std_logic_vector(15 downto 0);
        signal m_udp_dest_port           : out std_logic_vector(15 downto 0);
        signal m_udp_length              : out std_logic_vector(15 downto 0);
        signal m_udp_checksum            : out std_logic_vector(15 downto 0);
        signal m_udp_payload_axis_tdata  : out std_logic_vector(63 downto 0);
        signal m_udp_payload_axis_tkeep  : out std_logic_vector(7 downto 0);
        signal m_udp_payload_axis_tvalid : out std_logic;
        signal m_udp_payload_axis_tready : in  std_logic;
        signal m_udp_payload_axis_tlast  : out std_logic;
        signal m_udp_payload_axis_tuser  : out std_logic;

        --
        -- Status signals
        --
        signal busy                      : out std_logic
    );
end entity;

-- 
-- UDP Frame
-- 
--  Field                       Length
--  Destination MAC address     6 octets
--  Source MAC address          6 octets
--  Ethertype (0x0800)          2 octets
--  Version (4)                 4 bits
--  IHL (5-15)                  4 bits
--  DSCP (0)                    6 bits
--  ECN (0)                     2 bits
--  length                      2 octets
--  identification (0?)         2 octets
--  flags (010)                 3 bits
--  fragment offset (0)         13 bits
--  time to live (64?)          1 octet
--  protocol                    1 octet
--  header checksum             2 octets
--  source IP                   4 octets
--  destination IP              4 octets
--  options                     (IHL-5)*4 octets
-- 
--  source port                 2 octets
--  desination port             2 octets
--  length                      2 octets
--  checksum                    2 octets
-- 
--  payload                     length octets
-- 
-- This module receives a UDP frame with header fields in parallel and payload on
-- an AXI stream interface, calculates the length and checksum, then produces the
-- header fields in parallel along with the UDP payload in a separate AXI stream.
-- 

architecture rtl of udp_checksum_gen_64 is

    constant HEADER_FIFO_ADDR_WIDTH : integer := clog2(HEADER_FIFO_DEPTH);

    type t_state is (STATE_IDLE, STATE_SUM_HEADER, STATE_SUM_PAYLOAD, STATE_FINISH_SUM_1, STATE_FINISH_SUM_2);

    signal state_reg                      : t_state := STATE_IDLE;
    signal state_next                     : t_state;

    -- datapath control signals
    signal store_udp_hdr                  : std_logic;
    signal shift_payload_in               : std_logic;
    signal checksum_part                  : unsigned(31 downto 0);

    signal frame_ptr_reg                  : unsigned(15 downto 0) := (others => '0');
    signal frame_ptr_next                 : unsigned(15 downto 0);

    signal checksum_reg                   : unsigned(31 downto 0) := (others => '0');
    signal checksum_next                  : unsigned(31 downto 0);
    signal checksum_temp1_reg             : unsigned(16 downto 0) := (others => '0');
    signal checksum_temp1_next            : unsigned(16 downto 0);
    signal checksum_temp2_reg             : unsigned(16 downto 0) := (others => '0');
    signal checksum_temp2_next            : unsigned(16 downto 0);

    signal eth_dest_mac_reg               : std_logic_vector(47 downto 0) := (others => '0');
    signal eth_src_mac_reg                : std_logic_vector(47 downto 0) := (others => '0');
    signal eth_type_reg                   : std_logic_vector(15 downto 0) := (others => '0');
    signal ip_version_reg                 : std_logic_vector(3 downto 0)  := (others => '0');
    signal ip_ihl_reg                     : std_logic_vector(3 downto 0)  := (others => '0');
    signal ip_dscp_reg                    : std_logic_vector(5 downto 0)  := (others => '0');
    signal ip_ecn_reg                     : std_logic_vector(1 downto 0)  := (others => '0');
    signal ip_identification_reg          : std_logic_vector(15 downto 0) := (others => '0');
    signal ip_flags_reg                   : std_logic_vector(2 downto 0)  := (others => '0');
    signal ip_fragment_offset_reg         : std_logic_vector(12 downto 0) := (others => '0');
    signal ip_ttl_reg                     : std_logic_vector(7 downto 0)  := (others => '0');
    signal ip_header_checksum_reg         : std_logic_vector(15 downto 0) := (others => '0');
    signal ip_source_ip_reg               : std_logic_vector(31 downto 0) := (others => '0');
    signal ip_dest_ip_reg                 : std_logic_vector(31 downto 0) := (others => '0');
    signal udp_source_port_reg            : std_logic_vector(15 downto 0) := (others => '0');
    signal udp_dest_port_reg              : std_logic_vector(15 downto 0) := (others => '0');

    signal hdr_valid_reg                  : std_logic                     := '0';
    signal hdr_valid_next                 : std_logic;

    signal s_udp_hdr_ready_reg            : std_logic := '0';
    signal s_udp_hdr_ready_next           : std_logic;
    signal s_udp_payload_axis_tready_reg  : std_logic := '0';
    signal s_udp_payload_axis_tready_next : std_logic;

    signal busy_reg                       : std_logic := '0';

    -- UDP Payload FIFO
    signal s_udp_payload_fifo_tdata       : std_logic_vector(63 downto 0);
    signal s_udp_payload_fifo_tkeep       : std_logic_vector(7 downto 0);
    signal s_udp_payload_fifo_tvalid      : std_logic;
    signal s_udp_payload_fifo_tready      : std_logic;
    signal s_udp_payload_fifo_tlast       : std_logic;
    signal s_udp_payload_fifo_tuser       : std_logic;

    signal m_udp_payload_fifo_tdata       : std_logic_vector(63 downto 0);
    signal m_udp_payload_fifo_tkeep       : std_logic_vector(7 downto 0);
    signal m_udp_payload_fifo_tvalid      : std_logic;
    signal m_udp_payload_fifo_tready      : std_logic;
    signal m_udp_payload_fifo_tlast       : std_logic;
    signal m_udp_payload_fifo_tuser       : std_logic;

    -- UDP Header FIFO
    signal header_fifo_wr_ptr_reg         : unsigned(HEADER_FIFO_ADDR_WIDTH downto 0) := (others => '0');
    signal header_fifo_wr_ptr_next        : unsigned(HEADER_FIFO_ADDR_WIDTH downto 0);
    signal header_fifo_rd_ptr_reg         : unsigned(HEADER_FIFO_ADDR_WIDTH downto 0) := (others => '0');
    signal header_fifo_rd_ptr_next        : unsigned(HEADER_FIFO_ADDR_WIDTH downto 0);

    type t_mem is record
        eth_dest_mac       : std_logic_vector(47 downto 0);
        eth_src_mac        : std_logic_vector(47 downto 0);
        eth_type           : std_logic_vector(15 downto 0);
        ip_version         : std_logic_vector(3 downto 0);
        ip_ihl             : std_logic_vector(3 downto 0);
        ip_dscp            : std_logic_vector(5 downto 0);
        ip_ecn             : std_logic_vector(1 downto 0);
        ip_identification  : std_logic_vector(15 downto 0);
        ip_flags           : std_logic_vector(2 downto 0);
        ip_fragment_offset : std_logic_vector(12 downto 0);
        ip_ttl             : std_logic_vector(7 downto 0);
        ip_header_checksum : std_logic_vector(15 downto 0);
        ip_source_ip       : std_logic_vector(31 downto 0);
        ip_dest_ip         : std_logic_vector(31 downto 0);
        udp_source_port    : std_logic_vector(15 downto 0);
        udp_dest_port      : std_logic_vector(15 downto 0);
        udp_length         : std_logic_vector(15 downto 0);
        udp_checksum       : std_logic_vector(15 downto 0);
    end record;

    type a_mem is array (natural range <>) of t_mem;

    signal mem                      : a_mem((2 ** HEADER_FIFO_ADDR_WIDTH) - 1 downto 0);

    signal m_eth_dest_mac_reg       : std_logic_vector(47 downto 0) := (others => '0');
    signal m_eth_src_mac_reg        : std_logic_vector(47 downto 0) := (others => '0');
    signal m_eth_type_reg           : std_logic_vector(15 downto 0) := (others => '0');
    signal m_ip_version_reg         : std_logic_vector(3 downto 0)  := (others => '0');
    signal m_ip_ihl_reg             : std_logic_vector(3 downto 0)  := (others => '0');
    signal m_ip_dscp_reg            : std_logic_vector(5 downto 0)  := (others => '0');
    signal m_ip_ecn_reg             : std_logic_vector(1 downto 0)  := (others => '0');
    signal m_ip_identification_reg  : std_logic_vector(15 downto 0) := (others => '0');
    signal m_ip_flags_reg           : std_logic_vector(2 downto 0)  := (others => '0');
    signal m_ip_fragment_offset_reg : std_logic_vector(12 downto 0) := (others => '0');
    signal m_ip_ttl_reg             : std_logic_vector(7 downto 0)  := (others => '0');
    signal m_ip_header_checksum_reg : std_logic_vector(15 downto 0) := (others => '0');
    signal m_ip_source_ip_reg       : std_logic_vector(31 downto 0) := (others => '0');
    signal m_ip_dest_ip_reg         : std_logic_vector(31 downto 0) := (others => '0');
    signal m_udp_source_port_reg    : std_logic_vector(15 downto 0) := (others => '0');
    signal m_udp_dest_port_reg      : std_logic_vector(15 downto 0) := (others => '0');
    signal m_udp_length_reg         : std_logic_vector(15 downto 0) := (others => '0');
    signal m_udp_checksum_reg       : std_logic_vector(15 downto 0) := (others => '0');

    signal m_udp_hdr_valid_reg      : std_logic                     := '0';
    signal m_udp_hdr_valid_next     : std_logic;

    signal header_fifo_full         : std_logic;
    signal header_fifo_empty        : std_logic;
    signal header_fifo_ready        : std_logic;
    signal header_fifo_write        : std_logic;
    signal header_fifo_read         : std_logic;
begin

    --
    -- UDP Payload FIFO
    --
    payload_fifo : entity work.axis_fifo
        generic map(
            DEPTH       => PAYLOAD_FIFO_DEPTH,
            DATA_WIDTH  => 64,
            KEEP_ENABLE => 1,
            KEEP_WIDTH  => 8,
            LAST_ENABLE => 1,
            ID_ENABLE   => 0,
            DEST_ENABLE => 0,
            USER_ENABLE => 1,
            USER_WIDTH  => 1,
            FRAME_FIFO  => 0
        )
        port map(
            clk               => clk,
            rst               => rst,

            -- AXI input
            s_axis_tdata      => s_udp_payload_fifo_tdata,
            s_axis_tkeep      => s_udp_payload_fifo_tkeep,
            s_axis_tvalid     => s_udp_payload_fifo_tvalid,
            s_axis_tready     => s_udp_payload_fifo_tready,
            s_axis_tlast      => s_udp_payload_fifo_tlast,
            s_axis_tid        => x"00",
            s_axis_tdest      => x"00",
            s_axis_tuser(0)   => s_udp_payload_fifo_tuser,

            -- AXI output
            m_axis_tdata      => m_udp_payload_fifo_tdata,
            m_axis_tkeep      => m_udp_payload_fifo_tkeep,
            m_axis_tvalid     => m_udp_payload_fifo_tvalid,
            m_axis_tready     => m_udp_payload_fifo_tready,
            m_axis_tlast      => m_udp_payload_fifo_tlast,
            m_axis_tid        => open,
            m_axis_tdest      => open,
            m_axis_tuser(0)   => m_udp_payload_fifo_tuser,

            -- Status
            status_overflow   => open,
            status_bad_frame  => open,
            status_good_frame => open
        );

    s_udp_payload_fifo_tdata  <= s_udp_payload_axis_tdata;
    s_udp_payload_fifo_tkeep  <= s_udp_payload_axis_tkeep;
    s_udp_payload_fifo_tvalid <= s_udp_payload_axis_tvalid and shift_payload_in;
    s_udp_payload_axis_tready <= s_udp_payload_fifo_tready and shift_payload_in;
    s_udp_payload_fifo_tlast  <= s_udp_payload_axis_tlast;
    s_udp_payload_fifo_tuser  <= s_udp_payload_axis_tuser;

    m_udp_payload_axis_tdata  <= m_udp_payload_fifo_tdata;
    m_udp_payload_axis_tkeep  <= m_udp_payload_fifo_tkeep;
    m_udp_payload_axis_tvalid <= m_udp_payload_fifo_tvalid;
    m_udp_payload_fifo_tready <= m_udp_payload_axis_tready;
    m_udp_payload_axis_tlast  <= m_udp_payload_fifo_tlast;
    m_udp_payload_axis_tuser  <= m_udp_payload_fifo_tuser;

    --
    -- UDP Header FIFO
    --

    -- full when first MSB different but rest same
    header_fifo_full          <= '1' when fifo_is_full(header_fifo_wr_ptr_reg, header_fifo_rd_ptr_reg) else
                        '0';

    -- empty when pointers match exactly
    header_fifo_empty <= '1' when header_fifo_wr_ptr_reg = header_fifo_rd_ptr_reg else
                         '0';

    -- control signals
    header_fifo_ready    <= not(header_fifo_full);

    m_udp_hdr_valid      <= m_udp_hdr_valid_reg;

    m_eth_dest_mac       <= m_eth_dest_mac_reg;
    m_eth_src_mac        <= m_eth_src_mac_reg;
    m_eth_type           <= m_eth_type_reg;
    m_ip_version         <= m_ip_version_reg;
    m_ip_ihl             <= m_ip_ihl_reg;
    m_ip_dscp            <= m_ip_dscp_reg;
    m_ip_ecn             <= m_ip_ecn_reg;
    m_ip_length          <= std_logic_vector(unsigned(m_udp_length_reg) + 20);
    m_ip_identification  <= m_ip_identification_reg;
    m_ip_flags           <= m_ip_flags_reg;
    m_ip_fragment_offset <= m_ip_fragment_offset_reg;
    m_ip_ttl             <= m_ip_ttl_reg;
    m_ip_protocol        <= x"11";
    m_ip_header_checksum <= m_ip_header_checksum_reg;
    m_ip_source_ip       <= m_ip_source_ip_reg;
    m_ip_dest_ip         <= m_ip_dest_ip_reg;
    m_udp_source_port    <= m_udp_source_port_reg;
    m_udp_dest_port      <= m_udp_dest_port_reg;
    m_udp_length         <= m_udp_length_reg;
    m_udp_checksum       <= m_udp_checksum_reg;

    -- Write logic
    process (all) begin
        header_fifo_write       <= '0';

        header_fifo_wr_ptr_next <= header_fifo_wr_ptr_reg;

        if hdr_valid_reg = '1' then
            -- input data valid
            if header_fifo_full = '0' then
                -- not full, perform write
                header_fifo_write       <= '1';
                header_fifo_wr_ptr_next <= header_fifo_wr_ptr_reg + 1;
            end if;
        end if;
    end process;

    process (clk, rst) begin
        if rst = '1' then
            header_fifo_wr_ptr_reg <= (others => '0');
        elsif rising_edge(clk) then
            header_fifo_wr_ptr_reg <= header_fifo_wr_ptr_next;

            if header_fifo_write = '1' then
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).eth_dest_mac       <= eth_dest_mac_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).eth_src_mac        <= eth_src_mac_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).eth_type           <= eth_type_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_version         <= ip_version_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_ihl             <= ip_ihl_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_dscp            <= ip_dscp_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_ecn             <= ip_ecn_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_identification  <= ip_identification_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_flags           <= ip_flags_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_fragment_offset <= ip_fragment_offset_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_ttl             <= ip_ttl_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_header_checksum <= ip_header_checksum_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_source_ip       <= ip_source_ip_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_dest_ip         <= ip_dest_ip_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).udp_source_port    <= udp_source_port_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).udp_dest_port      <= udp_dest_port_reg;
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).udp_length         <= std_logic_vector(frame_ptr_reg);
                mem(to_integer(header_fifo_wr_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).udp_checksum       <= std_logic_vector(checksum_reg(15 downto 0));
            end if;
        end if;
    end process;

    -- Read logic
    process (all) begin
        header_fifo_read        <= '0';

        header_fifo_rd_ptr_next <= header_fifo_rd_ptr_reg;

        m_udp_hdr_valid_next    <= m_udp_hdr_valid_reg;

        if m_udp_hdr_ready = '1' or m_udp_hdr_valid = '0' then
            -- output data not valid OR currently being transferred
            if header_fifo_empty = '0' then
                -- not empty, perform read
                header_fifo_read        <= '1';
                m_udp_hdr_valid_next    <= '1';
                header_fifo_rd_ptr_next <= header_fifo_rd_ptr_reg + 1;
            else
                -- empty, invalidate
                m_udp_hdr_valid_next <= '0';
            end if;
        end if;
    end process;

    process (clk, rst) begin
        if rst = '1' then
            header_fifo_rd_ptr_reg <= (others => '0');
            m_udp_hdr_valid_reg    <= '0';
        elsif rising_edge(clk) then
            header_fifo_rd_ptr_reg <= header_fifo_rd_ptr_next;
            m_udp_hdr_valid_reg    <= m_udp_hdr_valid_next;

            if header_fifo_read = '1' then
                m_eth_dest_mac_reg       <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).eth_dest_mac;
                m_eth_src_mac_reg        <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).eth_src_mac;
                m_eth_type_reg           <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).eth_type;
                m_ip_version_reg         <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_version;
                m_ip_ihl_reg             <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_ihl;
                m_ip_dscp_reg            <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_dscp;
                m_ip_ecn_reg             <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_ecn;
                m_ip_identification_reg  <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_identification;
                m_ip_flags_reg           <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_flags;
                m_ip_fragment_offset_reg <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_fragment_offset;
                m_ip_ttl_reg             <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_ttl;
                m_ip_header_checksum_reg <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_header_checksum;
                m_ip_source_ip_reg       <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_source_ip;
                m_ip_dest_ip_reg         <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).ip_dest_ip;
                m_udp_source_port_reg    <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).udp_source_port;
                m_udp_dest_port_reg      <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).udp_dest_port;
                m_udp_length_reg         <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).udp_length;
                m_udp_checksum_reg       <= mem(to_integer(header_fifo_rd_ptr_reg(HEADER_FIFO_ADDR_WIDTH - 1 downto 0))).udp_checksum;
            end if;
        end if;
    end process;

    s_udp_hdr_ready <= s_udp_hdr_ready_reg;
    busy            <= busy_reg;

    process (all) is
        variable word_cnt : integer;
    begin
        state_next                     <= STATE_IDLE;

        s_udp_hdr_ready_next           <= '0';
        s_udp_payload_axis_tready_next <= '0';

        store_udp_hdr                  <= '0';
        shift_payload_in               <= '0';

        frame_ptr_next                 <= frame_ptr_reg;
        checksum_next                  <= checksum_reg;
        checksum_temp1_next            <= checksum_temp1_reg;
        checksum_temp2_next            <= checksum_temp2_reg;

        hdr_valid_next                 <= '0';

        case state_reg is
            when STATE_IDLE =>
                -- idle state
                s_udp_hdr_ready_next <= header_fifo_ready;

                if s_udp_hdr_ready = '1' and s_udp_hdr_valid = '1' then
                    store_udp_hdr        <= '1';
                    frame_ptr_next       <= (others => '0');
                    -- 16'h0011 <= zero padded type field
                    -- 16'h0010 <= header length times two
                    checksum_next        <= to_unsigned(16#11#, 32) + to_unsigned(16#10#, 32);
                    checksum_temp1_next  <= unsigned(s_ip_source_ip(31 downto 16));
                    checksum_temp2_next  <= unsigned(s_ip_source_ip(15 downto 0));
                    s_udp_hdr_ready_next <= '0';
                    state_next           <= STATE_SUM_HEADER;
                else
                    state_next <= STATE_IDLE;
                end if;

            when STATE_SUM_HEADER =>
                -- sum pseudo header and header
                checksum_next       <= checksum_reg + checksum_temp1_reg + checksum_temp2_reg;
                checksum_temp1_next <= unsigned(ip_dest_ip_reg(31 downto 16)) + unsigned(ip_dest_ip_reg(15 downto 0));
                checksum_temp2_next <= unsigned(udp_source_port_reg) + unsigned(udp_dest_port_reg);
                frame_ptr_next      <= to_unsigned(8, frame_ptr_next'length);
                state_next          <= STATE_SUM_PAYLOAD;

            when STATE_SUM_PAYLOAD =>
                -- sum payload
                shift_payload_in <= '1';

                if s_udp_payload_axis_tready = '1' and s_udp_payload_axis_tvalid = '1' then
                    word_cnt := 1;
                    for i in 1 to 8 loop
                        if s_udp_payload_axis_tkeep(8 - i) = '1' then
                            word_cnt := i;
                        end if;
                    end loop;

                    checksum_temp1_next <= (others => '0');
                    checksum_temp2_next <= (others => '0');

                    for i in 0 to 3 loop
                        if s_udp_payload_axis_tkeep(i) = '1' then
                            if i = 1 or i = 3 then
                                checksum_temp1_next <= checksum_temp1_next + unsigned(x"00" & s_udp_payload_axis_tdata(i * 8 + 7 downto i * 8));
                            else
                                checksum_temp1_next <= checksum_temp1_next + unsigned(s_udp_payload_axis_tdata(i * 8 + 7 downto i * 8) & x"00");
                            end if;
                        end if;
                    end loop;

                    for i in 4 to 7 loop
                        if s_udp_payload_axis_tkeep(i) = '1' then
                            if i = 5 or i = 7 then
                                checksum_temp2_next <= checksum_temp2_next + unsigned(x"00" & s_udp_payload_axis_tdata(i * 8 + 7 downto i * 8));
                            else
                                checksum_temp2_next <= checksum_temp2_next + unsigned(s_udp_payload_axis_tdata(i * 8 + 7 downto i * 8) & x"00");
                            end if;
                        end if;
                    end loop;

                    -- add length * 2 (two copies of length field in pseudo header)
                    checksum_next  <= checksum_reg + checksum_temp1_reg + checksum_temp2_reg + (word_cnt * 2);

                    frame_ptr_next <= frame_ptr_reg + to_unsigned(word_cnt, frame_ptr_reg'length);

                    if s_udp_payload_axis_tlast = '1' then
                        state_next <= STATE_FINISH_SUM_1;
                    else
                        state_next <= STATE_SUM_PAYLOAD;
                    end if;
                else
                    state_next <= STATE_SUM_PAYLOAD;
                end if;

            when STATE_FINISH_SUM_1 =>
                -- empty pipeline
                checksum_next <= checksum_reg + checksum_temp1_reg + checksum_temp2_reg;
                state_next    <= STATE_FINISH_SUM_2;

            when STATE_FINISH_SUM_2 =>
                -- add MSW (twice!) for proper ones complement sum
                checksum_part  <= checksum_reg(15 downto 0) + checksum_reg(31 downto 16);
                checksum_next  <= not(checksum_part(15 downto 0) + checksum_part(16));
                hdr_valid_next <= '1';
                state_next     <= STATE_IDLE;
        end case;
    end process;

    process (rst, clk) begin
        if rst = '1' then
            state_reg                     <= STATE_IDLE;
            s_udp_hdr_ready_reg           <= '0';
            s_udp_payload_axis_tready_reg <= '0';
            hdr_valid_reg                 <= '0';
            busy_reg                      <= '0';
        elsif rising_edge(clk) then
            state_reg                     <= state_next;

            s_udp_hdr_ready_reg           <= s_udp_hdr_ready_next;
            s_udp_payload_axis_tready_reg <= s_udp_payload_axis_tready_next;

            hdr_valid_reg                 <= hdr_valid_next;

            busy_reg                      <= '1' when state_next /= STATE_IDLE else
                        '0';

            frame_ptr_reg      <= frame_ptr_next;
            checksum_reg       <= checksum_next;
            checksum_temp1_reg <= checksum_temp1_next;
            checksum_temp2_reg <= checksum_temp2_next;

            -- datapath
            if store_udp_hdr = '1' then
                eth_dest_mac_reg       <= s_eth_dest_mac;
                eth_src_mac_reg        <= s_eth_src_mac;
                eth_type_reg           <= s_eth_type;
                ip_version_reg         <= s_ip_version;
                ip_ihl_reg             <= s_ip_ihl;
                ip_dscp_reg            <= s_ip_dscp;
                ip_ecn_reg             <= s_ip_ecn;
                ip_identification_reg  <= s_ip_identification;
                ip_flags_reg           <= s_ip_flags;
                ip_fragment_offset_reg <= s_ip_fragment_offset;
                ip_ttl_reg             <= s_ip_ttl;
                ip_header_checksum_reg <= s_ip_header_checksum;
                ip_source_ip_reg       <= s_ip_source_ip;
                ip_dest_ip_reg         <= s_ip_dest_ip;
                udp_source_port_reg    <= s_udp_source_port;
                udp_dest_port_reg      <= s_udp_dest_port;
            end if;
        end if;
    end process;

end architecture;