-- 
-- Copyright (c) 2014-2018 Alex Forencich
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

-- 
-- IPv4 and ARP block with UDP support, ethernet frame interface (64 bit datapath)
-- 
entity udp_complete_64 is
    generic (
        ARP_CACHE_ADDR_WIDTH            : integer := 9;
        ARP_REQUEST_RETRY_COUNT         : integer := 4;
        ARP_REQUEST_RETRY_INTERVAL      : integer := 125000000 * 2;
        ARP_REQUEST_TIMEOUT             : integer := 125000000 * 30;
        UDP_CHECKSUM_GEN_ENABLE         : integer := 1;
        UDP_CHECKSUM_PAYLOAD_FIFO_DEPTH : integer := 2048;
        UDP_CHECKSUM_HEADER_FIFO_DEPTH  : integer := 8
    );
    port (
        signal clk                                    : in  std_logic;
        signal rst                                    : in  std_logic;

        --
        -- Ethernet frame input
        --
        signal s_eth_hdr_valid                        : in  std_logic;
        signal s_eth_hdr_ready                        : out std_logic;
        signal s_eth_dest_mac                         : in  std_logic_vector(47 downto 0);
        signal s_eth_src_mac                          : in  std_logic_vector(47 downto 0);
        signal s_eth_type                             : in  std_logic_vector(15 downto 0);
        signal s_eth_payload_axis_tdata               : in  std_logic_vector(63 downto 0);
        signal s_eth_payload_axis_tkeep               : in  std_logic_vector(7 downto 0);
        signal s_eth_payload_axis_tvalid              : in  std_logic;
        signal s_eth_payload_axis_tready              : out std_logic;
        signal s_eth_payload_axis_tlast               : in  std_logic;
        signal s_eth_payload_axis_tuser               : in  std_logic;

        --
        -- Ethernet frame output
        --
        signal m_eth_hdr_valid                        : out std_logic;
        signal m_eth_hdr_ready                        : in  std_logic;
        signal m_eth_dest_mac                         : out std_logic_vector(47 downto 0);
        signal m_eth_src_mac                          : out std_logic_vector(47 downto 0);
        signal m_eth_type                             : out std_logic_vector(15 downto 0);
        signal m_eth_payload_axis_tdata               : out std_logic_vector(63 downto 0);
        signal m_eth_payload_axis_tkeep               : out std_logic_vector(7 downto 0);
        signal m_eth_payload_axis_tvalid              : out std_logic;
        signal m_eth_payload_axis_tready              : in  std_logic;
        signal m_eth_payload_axis_tlast               : out std_logic;
        signal m_eth_payload_axis_tuser               : out std_logic;

        --
        --IP input
        --
        signal s_ip_hdr_valid                         : in  std_logic;
        signal s_ip_hdr_ready                         : out std_logic;
        signal s_ip_dscp                              : in  std_logic_vector(5 downto 0);
        signal s_ip_ecn                               : in  std_logic_vector(1 downto 0);
        signal s_ip_length                            : in  std_logic_vector(15 downto 0);
        signal s_ip_ttl                               : in  std_logic_vector(7 downto 0);
        signal s_ip_protocol                          : in  std_logic_vector(7 downto 0);
        signal s_ip_source_ip                         : in  std_logic_vector(31 downto 0);
        signal s_ip_dest_ip                           : in  std_logic_vector(31 downto 0);
        signal s_ip_payload_axis_tdata                : in  std_logic_vector(63 downto 0);
        signal s_ip_payload_axis_tkeep                : in  std_logic_vector(7 downto 0);
        signal s_ip_payload_axis_tvalid               : in  std_logic;
        signal s_ip_payload_axis_tready               : out std_logic;
        signal s_ip_payload_axis_tlast                : in  std_logic;
        signal s_ip_payload_axis_tuser                : in  std_logic;

        --
        -- IP output
        --
        signal m_ip_hdr_valid                         : out std_logic;
        signal m_ip_hdr_ready                         : in  std_logic;
        signal m_ip_eth_dest_mac                      : out std_logic_vector(47 downto 0);
        signal m_ip_eth_src_mac                       : out std_logic_vector(47 downto 0);
        signal m_ip_eth_type                          : out std_logic_vector(15 downto 0);
        signal m_ip_version                           : out std_logic_vector(3 downto 0);
        signal m_ip_ihl                               : out std_logic_vector(3 downto 0);
        signal m_ip_dscp                              : out std_logic_vector(5 downto 0);
        signal m_ip_ecn                               : out std_logic_vector(1 downto 0);
        signal m_ip_length                            : out std_logic_vector(15 downto 0);
        signal m_ip_identification                    : out std_logic_vector(15 downto 0);
        signal m_ip_flags                             : out std_logic_vector(2 downto 0);
        signal m_ip_fragment_offset                   : out std_logic_vector(12 downto 0);
        signal m_ip_ttl                               : out std_logic_vector(7 downto 0);
        signal m_ip_protocol                          : out std_logic_vector(7 downto 0);
        signal m_ip_header_checksum                   : out std_logic_vector(15 downto 0);
        signal m_ip_source_ip                         : out std_logic_vector(31 downto 0);
        signal m_ip_dest_ip                           : out std_logic_vector(31 downto 0);
        signal m_ip_payload_axis_tdata                : out std_logic_vector(63 downto 0);
        signal m_ip_payload_axis_tkeep                : out std_logic_vector(7 downto 0);
        signal m_ip_payload_axis_tvalid               : out std_logic;
        signal m_ip_payload_axis_tready               : in  std_logic;
        signal m_ip_payload_axis_tlast                : out std_logic;
        signal m_ip_payload_axis_tuser                : out std_logic;

        -- 
        -- UDP input
        -- 
        signal s_udp_hdr_valid                        : in  std_logic;
        signal s_udp_hdr_ready                        : out std_logic;
        signal s_udp_ip_dscp                          : in  std_logic_vector(5 downto 0);
        signal s_udp_ip_ecn                           : in  std_logic_vector(1 downto 0);
        signal s_udp_ip_ttl                           : in  std_logic_vector(7 downto 0);
        signal s_udp_ip_source_ip                     : in  std_logic_vector(31 downto 0);
        signal s_udp_ip_dest_ip                       : in  std_logic_vector(31 downto 0);
        signal s_udp_source_port                      : in  std_logic_vector(15 downto 0);
        signal s_udp_dest_port                        : in  std_logic_vector(15 downto 0);
        signal s_udp_length                           : in  std_logic_vector(15 downto 0);
        signal s_udp_checksum                         : in  std_logic_vector(15 downto 0);
        signal s_udp_payload_axis_tdata               : in  std_logic_vector(63 downto 0);
        signal s_udp_payload_axis_tkeep               : in  std_logic_vector(7 downto 0);
        signal s_udp_payload_axis_tvalid              : in  std_logic;
        signal s_udp_payload_axis_tready              : out std_logic;
        signal s_udp_payload_axis_tlast               : in  std_logic;
        signal s_udp_payload_axis_tuser               : in  std_logic;

        --
        -- UDP output
        --
        signal m_udp_hdr_valid                        : out std_logic;
        signal m_udp_hdr_ready                        : in  std_logic;
        signal m_udp_eth_dest_mac                     : out std_logic_vector(47 downto 0);
        signal m_udp_eth_src_mac                      : out std_logic_vector(47 downto 0);
        signal m_udp_eth_type                         : out std_logic_vector(15 downto 0);
        signal m_udp_ip_version                       : out std_logic_vector(3 downto 0);
        signal m_udp_ip_ihl                           : out std_logic_vector(3 downto 0);
        signal m_udp_ip_dscp                          : out std_logic_vector(5 downto 0);
        signal m_udp_ip_ecn                           : out std_logic_vector(1 downto 0);
        signal m_udp_ip_length                        : out std_logic_vector(15 downto 0);
        signal m_udp_ip_identification                : out std_logic_vector(15 downto 0);
        signal m_udp_ip_flags                         : out std_logic_vector(2 downto 0);
        signal m_udp_ip_fragment_offset               : out std_logic_vector(12 downto 0);
        signal m_udp_ip_ttl                           : out std_logic_vector(7 downto 0);
        signal m_udp_ip_protocol                      : out std_logic_vector(7 downto 0);
        signal m_udp_ip_header_checksum               : out std_logic_vector(15 downto 0);
        signal m_udp_ip_source_ip                     : out std_logic_vector(31 downto 0);
        signal m_udp_ip_dest_ip                       : out std_logic_vector(31 downto 0);
        signal m_udp_source_port                      : out std_logic_vector(15 downto 0);
        signal m_udp_dest_port                        : out std_logic_vector(15 downto 0);
        signal m_udp_length                           : out std_logic_vector(15 downto 0);
        signal m_udp_checksum                         : out std_logic_vector(15 downto 0);
        signal m_udp_payload_axis_tdata               : out std_logic_vector(63 downto 0);
        signal m_udp_payload_axis_tkeep               : out std_logic_vector(7 downto 0);
        signal m_udp_payload_axis_tvalid              : out std_logic;
        signal m_udp_payload_axis_tready              : in  std_logic;
        signal m_udp_payload_axis_tlast               : out std_logic;
        signal m_udp_payload_axis_tuser               : out std_logic;

        -- 
        -- Status
        -- 
        signal ip_rx_busy                             : out std_logic;
        signal ip_tx_busy                             : out std_logic;
        signal udp_rx_busy                            : out std_logic;
        signal udp_tx_busy                            : out std_logic;
        signal ip_rx_error_header_early_termination   : out std_logic;
        signal ip_rx_error_payload_early_termination  : out std_logic;
        signal ip_rx_error_invalid_header             : out std_logic;
        signal ip_rx_error_invalid_checksum           : out std_logic;
        signal ip_tx_error_payload_early_termination  : out std_logic;
        signal ip_tx_error_arp_failed                 : out std_logic;
        signal udp_rx_error_header_early_termination  : out std_logic;
        signal udp_rx_error_payload_early_termination : out std_logic;
        signal udp_tx_error_payload_early_termination : out std_logic;

        --
        -- Configuration
        --
        signal local_mac                              : in  std_logic_vector(47 downto 0);
        signal local_ip                               : in  std_logic_vector(31 downto 0);
        signal gateway_ip                             : in  std_logic_vector(31 downto 0);
        signal subnet_mask                            : in  std_logic_vector(31 downto 0);
        signal clear_arp_cache                        : in  std_logic
    );
end entity;

architecture rtl of udp_complete_64 is
    signal ip_rx_ip_hdr_valid            : std_logic;
    signal ip_rx_ip_hdr_ready            : std_logic;
    signal ip_rx_ip_eth_dest_mac         : std_logic_vector(47 downto 0);
    signal ip_rx_ip_eth_src_mac          : std_logic_vector(47 downto 0);
    signal ip_rx_ip_eth_type             : std_logic_vector(15 downto 0);
    signal ip_rx_ip_version              : std_logic_vector(3 downto 0);
    signal ip_rx_ip_ihl                  : std_logic_vector(3 downto 0);
    signal ip_rx_ip_dscp                 : std_logic_vector(5 downto 0);
    signal ip_rx_ip_ecn                  : std_logic_vector(1 downto 0);
    signal ip_rx_ip_length               : std_logic_vector(15 downto 0);
    signal ip_rx_ip_identification       : std_logic_vector(15 downto 0);
    signal ip_rx_ip_flags                : std_logic_vector(2 downto 0);
    signal ip_rx_ip_fragment_offset      : std_logic_vector(12 downto 0);
    signal ip_rx_ip_ttl                  : std_logic_vector(7 downto 0);
    signal ip_rx_ip_protocol             : std_logic_vector(7 downto 0);
    signal ip_rx_ip_header_checksum      : std_logic_vector(15 downto 0);
    signal ip_rx_ip_source_ip            : std_logic_vector(31 downto 0);
    signal ip_rx_ip_dest_ip              : std_logic_vector(31 downto 0);
    signal ip_rx_ip_payload_axis_tdata   : std_logic_vector(63 downto 0);
    signal ip_rx_ip_payload_axis_tkeep   : std_logic_vector(7 downto 0);
    signal ip_rx_ip_payload_axis_tvalid  : std_logic;
    signal ip_rx_ip_payload_axis_tlast   : std_logic;
    signal ip_rx_ip_payload_axis_tuser   : std_logic;
    signal ip_rx_ip_payload_axis_tready  : std_logic;

    signal ip_tx_ip_hdr_valid            : std_logic;
    signal ip_tx_ip_hdr_ready            : std_logic;
    signal ip_tx_ip_dscp                 : std_logic_vector(5 downto 0);
    signal ip_tx_ip_ecn                  : std_logic_vector(1 downto 0);
    signal ip_tx_ip_length               : std_logic_vector(15 downto 0);
    signal ip_tx_ip_ttl                  : std_logic_vector(7 downto 0);
    signal ip_tx_ip_protocol             : std_logic_vector(7 downto 0);
    signal ip_tx_ip_source_ip            : std_logic_vector(31 downto 0);
    signal ip_tx_ip_dest_ip              : std_logic_vector(31 downto 0);
    signal ip_tx_ip_payload_axis_tdata   : std_logic_vector(63 downto 0);
    signal ip_tx_ip_payload_axis_tkeep   : std_logic_vector(7 downto 0);
    signal ip_tx_ip_payload_axis_tvalid  : std_logic;
    signal ip_tx_ip_payload_axis_tlast   : std_logic;
    signal ip_tx_ip_payload_axis_tuser   : std_logic;
    signal ip_tx_ip_payload_axis_tready  : std_logic;

    signal udp_rx_ip_hdr_valid           : std_logic;
    signal udp_rx_ip_hdr_ready           : std_logic;
    signal udp_rx_ip_eth_dest_mac        : std_logic_vector(47 downto 0);
    signal udp_rx_ip_eth_src_mac         : std_logic_vector(47 downto 0);
    signal udp_rx_ip_eth_type            : std_logic_vector(15 downto 0);
    signal udp_rx_ip_version             : std_logic_vector(3 downto 0);
    signal udp_rx_ip_ihl                 : std_logic_vector(3 downto 0);
    signal udp_rx_ip_dscp                : std_logic_vector(5 downto 0);
    signal udp_rx_ip_ecn                 : std_logic_vector(1 downto 0);
    signal udp_rx_ip_length              : std_logic_vector(15 downto 0);
    signal udp_rx_ip_identification      : std_logic_vector(15 downto 0);
    signal udp_rx_ip_flags               : std_logic_vector(2 downto 0);
    signal udp_rx_ip_fragment_offset     : std_logic_vector(12 downto 0);
    signal udp_rx_ip_ttl                 : std_logic_vector(7 downto 0);
    signal udp_rx_ip_protocol            : std_logic_vector(7 downto 0);
    signal udp_rx_ip_header_checksum     : std_logic_vector(15 downto 0);
    signal udp_rx_ip_source_ip           : std_logic_vector(31 downto 0);
    signal udp_rx_ip_dest_ip             : std_logic_vector(31 downto 0);
    signal udp_rx_ip_payload_axis_tdata  : std_logic_vector(63 downto 0);
    signal udp_rx_ip_payload_axis_tkeep  : std_logic_vector(7 downto 0);
    signal udp_rx_ip_payload_axis_tvalid : std_logic;
    signal udp_rx_ip_payload_axis_tlast  : std_logic;
    signal udp_rx_ip_payload_axis_tuser  : std_logic;
    signal udp_rx_ip_payload_axis_tready : std_logic;

    signal udp_tx_ip_hdr_valid           : std_logic;
    signal udp_tx_ip_hdr_ready           : std_logic;
    signal udp_tx_ip_dscp                : std_logic_vector(5 downto 0);
    signal udp_tx_ip_ecn                 : std_logic_vector(1 downto 0);
    signal udp_tx_ip_length              : std_logic_vector(15 downto 0);
    signal udp_tx_ip_ttl                 : std_logic_vector(7 downto 0);
    signal udp_tx_ip_protocol            : std_logic_vector(7 downto 0);
    signal udp_tx_ip_source_ip           : std_logic_vector(31 downto 0);
    signal udp_tx_ip_dest_ip             : std_logic_vector(31 downto 0);
    signal udp_tx_ip_payload_axis_tdata  : std_logic_vector(63 downto 0);
    signal udp_tx_ip_payload_axis_tkeep  : std_logic_vector(7 downto 0);
    signal udp_tx_ip_payload_axis_tvalid : std_logic;
    signal udp_tx_ip_payload_axis_tlast  : std_logic;
    signal udp_tx_ip_payload_axis_tuser  : std_logic;
    signal udp_tx_ip_payload_axis_tready : std_logic;

    signal s_select_udp                  : std_logic;
    signal s_select_ip                   : std_logic;

    signal s_select_udp_reg              : std_logic;
    signal s_select_ip_reg               : std_logic;

    constant const_0                     : std_logic_vector(47 downto 0) := (others => '0');
begin

    -- 
    -- Input classifier (ip_protocol)
    -- 
    s_select_udp <= '1' when ip_rx_ip_protocol = x"11" else
                    '0';
    s_select_ip      <= not s_select_udp;

    s_select_udp_reg <= '0';
    s_select_ip_reg  <= '0';

    process (clk) begin
        if rising_edge(clk) then
            if (rst) then
                s_select_udp_reg <= '0';
                s_select_ip_reg  <= '0';
            else
                if ip_rx_ip_payload_axis_tvalid = '1' then
                    if (s_select_udp_reg = '0' and s_select_ip_reg = '0') or
                        (ip_rx_ip_payload_axis_tvalid = '1' and ip_rx_ip_payload_axis_tready = '1' and ip_rx_ip_payload_axis_tlast = '1') then
                        s_select_udp_reg <= s_select_udp;
                        s_select_ip_reg  <= s_select_ip;
                    end if;
                else
                    s_select_udp_reg <= '0';
                    s_select_ip_reg  <= '0';
                end if;
            end if;
        end if;
    end process;

    -- IP frame to UDP module
    udp_rx_ip_hdr_valid           <= s_select_udp and ip_rx_ip_hdr_valid;
    udp_rx_ip_eth_dest_mac        <= ip_rx_ip_eth_dest_mac;
    udp_rx_ip_eth_src_mac         <= ip_rx_ip_eth_src_mac;
    udp_rx_ip_eth_type            <= ip_rx_ip_eth_type;
    udp_rx_ip_version             <= ip_rx_ip_version;
    udp_rx_ip_ihl                 <= ip_rx_ip_ihl;
    udp_rx_ip_dscp                <= ip_rx_ip_dscp;
    udp_rx_ip_ecn                 <= ip_rx_ip_ecn;
    udp_rx_ip_length              <= ip_rx_ip_length;
    udp_rx_ip_identification      <= ip_rx_ip_identification;
    udp_rx_ip_flags               <= ip_rx_ip_flags;
    udp_rx_ip_fragment_offset     <= ip_rx_ip_fragment_offset;
    udp_rx_ip_ttl                 <= ip_rx_ip_ttl;
    udp_rx_ip_protocol            <= x"11";
    udp_rx_ip_header_checksum     <= ip_rx_ip_header_checksum;
    udp_rx_ip_source_ip           <= ip_rx_ip_source_ip;
    udp_rx_ip_dest_ip             <= ip_rx_ip_dest_ip;
    udp_rx_ip_payload_axis_tdata  <= ip_rx_ip_payload_axis_tdata;
    udp_rx_ip_payload_axis_tkeep  <= ip_rx_ip_payload_axis_tkeep;
    udp_rx_ip_payload_axis_tvalid <= s_select_udp_reg and ip_rx_ip_payload_axis_tvalid;
    udp_rx_ip_payload_axis_tlast  <= ip_rx_ip_payload_axis_tlast;
    udp_rx_ip_payload_axis_tuser  <= ip_rx_ip_payload_axis_tuser;

    -- External IP frame output
    m_ip_hdr_valid                <= s_select_ip and ip_rx_ip_hdr_valid;
    m_ip_eth_dest_mac             <= ip_rx_ip_eth_dest_mac;
    m_ip_eth_src_mac              <= ip_rx_ip_eth_src_mac;
    m_ip_eth_type                 <= ip_rx_ip_eth_type;
    m_ip_version                  <= ip_rx_ip_version;
    m_ip_ihl                      <= ip_rx_ip_ihl;
    m_ip_dscp                     <= ip_rx_ip_dscp;
    m_ip_ecn                      <= ip_rx_ip_ecn;
    m_ip_length                   <= ip_rx_ip_length;
    m_ip_identification           <= ip_rx_ip_identification;
    m_ip_flags                    <= ip_rx_ip_flags;
    m_ip_fragment_offset          <= ip_rx_ip_fragment_offset;
    m_ip_ttl                      <= ip_rx_ip_ttl;
    m_ip_protocol                 <= ip_rx_ip_protocol;
    m_ip_header_checksum          <= ip_rx_ip_header_checksum;
    m_ip_source_ip                <= ip_rx_ip_source_ip;
    m_ip_dest_ip                  <= ip_rx_ip_dest_ip;
    m_ip_payload_axis_tdata       <= ip_rx_ip_payload_axis_tdata;
    m_ip_payload_axis_tkeep       <= ip_rx_ip_payload_axis_tkeep;
    m_ip_payload_axis_tvalid      <= s_select_ip_reg and ip_rx_ip_payload_axis_tvalid;
    m_ip_payload_axis_tlast       <= ip_rx_ip_payload_axis_tlast;
    m_ip_payload_axis_tuser       <= ip_rx_ip_payload_axis_tuser;

    ip_rx_ip_hdr_ready            <= (s_select_udp and udp_rx_ip_hdr_ready) or
                          (s_select_ip and m_ip_hdr_ready);

    ip_rx_ip_payload_axis_tready <= (s_select_udp_reg and udp_rx_ip_payload_axis_tready) or
                                    (s_select_ip_reg and m_ip_payload_axis_tready);

    --
    -- Output arbiter
    --
    ip_arb_mux_inst : entity work.ip_arb_mux
        generic map(
            S_COUNT               => 2,
            DATA_WIDTH            => 64,
            KEEP_ENABLE           => 1,
            ID_ENABLE             => 0,
            DEST_ENABLE           => 0,
            USER_ENABLE           => 1,
            USER_WIDTH            => 1,
            ARB_TYPE_ROUND_ROBIN  => 0,
            ARB_LSB_HIGH_PRIORITY => 1
        )
        port map
        (
            clk                      => clk,
            rst                      => rst,

            -- IP frame inputs
            s_ip_hdr_valid           => s_ip_hdr_valid & udp_tx_ip_hdr_valid,
            s_ip_hdr_ready           => s_ip_hdr_ready & udp_tx_ip_hdr_ready,
            s_eth_dest_mac           => const_0(47 downto 0) & const_0(47 downto 0),
            s_eth_src_mac            => const_0(47 downto 0) & const_0(47 downto 0),
            s_eth_type               => const_0(15 downto 0) & const_0(15 downto 0),
            s_ip_version             => const_0(3 downto 0) & const_0(3 downto 0),
            s_ip_ihl                 => const_0(3 downto 0) & const_0(3 downto 0),
            s_ip_dscp                => s_ip_dscp & udp_tx_ip_dscp,
            s_ip_ecn                 => s_ip_ecn & udp_tx_ip_ecn,
            s_ip_length              => s_ip_length & udp_tx_ip_length,
            s_ip_identification      => const_0(15 downto 0) & const_0(15 downto 0),
            s_ip_flags               => const_0(2 downto 0) & const_0(2 downto 0),
            s_ip_fragment_offset     => const_0(12 downto 0) & const_0(12 downto 0),
            s_ip_ttl                 => s_ip_ttl & udp_tx_ip_ttl,
            s_ip_protocol            => s_ip_protocol & udp_tx_ip_protocol,
            s_ip_header_checksum     => const_0(15 downto 0) & const_0(15 downto 0),
            s_ip_source_ip           => s_ip_source_ip & udp_tx_ip_source_ip,
            s_ip_dest_ip             => s_ip_dest_ip & udp_tx_ip_dest_ip,
            s_ip_payload_axis_tdata  => s_ip_payload_axis_tdata & udp_tx_ip_payload_axis_tdata,
            s_ip_payload_axis_tkeep  => s_ip_payload_axis_tkeep & udp_tx_ip_payload_axis_tkeep,
            s_ip_payload_axis_tvalid => s_ip_payload_axis_tvalid & udp_tx_ip_payload_axis_tvalid,
            s_ip_payload_axis_tready => s_ip_payload_axis_tready & udp_tx_ip_payload_axis_tready,
            s_ip_payload_axis_tlast  => s_ip_payload_axis_tlast & udp_tx_ip_payload_axis_tlast,
            s_ip_payload_axis_tid    => const_0(7 downto 0) & const_0(7 downto 0),
            s_ip_payload_axis_tdest  => const_0(7 downto 0) & const_0(7 downto 0),
            s_ip_payload_axis_tuser  => s_ip_payload_axis_tuser & udp_tx_ip_payload_axis_tuser,

            -- IP frame output
            m_ip_hdr_valid           => ip_tx_ip_hdr_valid,
            m_ip_hdr_ready           => ip_tx_ip_hdr_ready,
            m_eth_dest_mac           => open,
            m_eth_src_mac            => open,
            m_eth_type               => open,
            m_ip_version             => open,
            m_ip_ihl                 => open,
            m_ip_dscp                => ip_tx_ip_dscp,
            m_ip_ecn                 => ip_tx_ip_ecn,
            m_ip_length              => ip_tx_ip_length,
            m_ip_identification      => open,
            m_ip_flags               => open,
            m_ip_fragment_offset     => open,
            m_ip_ttl                 => ip_tx_ip_ttl,
            m_ip_protocol            => ip_tx_ip_protocol,
            m_ip_header_checksum     => open,
            m_ip_source_ip           => ip_tx_ip_source_ip,
            m_ip_dest_ip             => ip_tx_ip_dest_ip,
            m_ip_payload_axis_tdata  => ip_tx_ip_payload_axis_tdata,
            m_ip_payload_axis_tkeep  => ip_tx_ip_payload_axis_tkeep,
            m_ip_payload_axis_tvalid => ip_tx_ip_payload_axis_tvalid,
            m_ip_payload_axis_tready => ip_tx_ip_payload_axis_tready,
            m_ip_payload_axis_tlast  => ip_tx_ip_payload_axis_tlast,
            m_ip_payload_axis_tid    => open,
            m_ip_payload_axis_tdest  => open,
            m_ip_payload_axis_tuser  => ip_tx_ip_payload_axis_tuser
        );

    -- 
    -- IP stack
    -- 
    ip_complete_64_inst : entity work.ip_complete_64
        generic map(
            ARP_CACHE_ADDR_WIDTH       => ARP_CACHE_ADDR_WIDTH,
            ARP_REQUEST_RETRY_COUNT    => ARP_REQUEST_RETRY_COUNT,
            ARP_REQUEST_RETRY_INTERVAL => ARP_REQUEST_RETRY_INTERVAL,
            ARP_REQUEST_TIMEOUT        => ARP_REQUEST_TIMEOUT
        )
        port map
        (
            clk                                => clk,
            rst                                => rst,

            -- Ethernet frame input
            s_eth_hdr_valid                    => s_eth_hdr_valid,
            s_eth_hdr_ready                    => s_eth_hdr_ready,
            s_eth_dest_mac                     => s_eth_dest_mac,
            s_eth_src_mac                      => s_eth_src_mac,
            s_eth_type                         => s_eth_type,
            s_eth_payload_axis_tdata           => s_eth_payload_axis_tdata,
            s_eth_payload_axis_tkeep           => s_eth_payload_axis_tkeep,
            s_eth_payload_axis_tvalid          => s_eth_payload_axis_tvalid,
            s_eth_payload_axis_tready          => s_eth_payload_axis_tready,
            s_eth_payload_axis_tlast           => s_eth_payload_axis_tlast,
            s_eth_payload_axis_tuser           => s_eth_payload_axis_tuser,

            -- Ethernet frame output
            m_eth_hdr_valid                    => m_eth_hdr_valid,
            m_eth_hdr_ready                    => m_eth_hdr_ready,
            m_eth_dest_mac                     => m_eth_dest_mac,
            m_eth_src_mac                      => m_eth_src_mac,
            m_eth_type                         => m_eth_type,
            m_eth_payload_axis_tdata           => m_eth_payload_axis_tdata,
            m_eth_payload_axis_tkeep           => m_eth_payload_axis_tkeep,
            m_eth_payload_axis_tvalid          => m_eth_payload_axis_tvalid,
            m_eth_payload_axis_tready          => m_eth_payload_axis_tready,
            m_eth_payload_axis_tlast           => m_eth_payload_axis_tlast,
            m_eth_payload_axis_tuser           => m_eth_payload_axis_tuser,

            -- IP frame input
            s_ip_hdr_valid                     => ip_tx_ip_hdr_valid,
            s_ip_hdr_ready                     => ip_tx_ip_hdr_ready,
            s_ip_dscp                          => ip_tx_ip_dscp,
            s_ip_ecn                           => ip_tx_ip_ecn,
            s_ip_length                        => ip_tx_ip_length,
            s_ip_ttl                           => ip_tx_ip_ttl,
            s_ip_protocol                      => ip_tx_ip_protocol,
            s_ip_source_ip                     => ip_tx_ip_source_ip,
            s_ip_dest_ip                       => ip_tx_ip_dest_ip,
            s_ip_payload_axis_tdata            => ip_tx_ip_payload_axis_tdata,
            s_ip_payload_axis_tkeep            => ip_tx_ip_payload_axis_tkeep,
            s_ip_payload_axis_tvalid           => ip_tx_ip_payload_axis_tvalid,
            s_ip_payload_axis_tready           => ip_tx_ip_payload_axis_tready,
            s_ip_payload_axis_tlast            => ip_tx_ip_payload_axis_tlast,
            s_ip_payload_axis_tuser            => ip_tx_ip_payload_axis_tuser,

            -- IP frame output
            m_ip_hdr_valid                     => ip_rx_ip_hdr_valid,
            m_ip_hdr_ready                     => ip_rx_ip_hdr_ready,
            m_ip_eth_dest_mac                  => ip_rx_ip_eth_dest_mac,
            m_ip_eth_src_mac                   => ip_rx_ip_eth_src_mac,
            m_ip_eth_type                      => ip_rx_ip_eth_type,
            m_ip_version                       => ip_rx_ip_version,
            m_ip_ihl                           => ip_rx_ip_ihl,
            m_ip_dscp                          => ip_rx_ip_dscp,
            m_ip_ecn                           => ip_rx_ip_ecn,
            m_ip_length                        => ip_rx_ip_length,
            m_ip_identification                => ip_rx_ip_identification,
            m_ip_flags                         => ip_rx_ip_flags,
            m_ip_fragment_offset               => ip_rx_ip_fragment_offset,
            m_ip_ttl                           => ip_rx_ip_ttl,
            m_ip_protocol                      => ip_rx_ip_protocol,
            m_ip_header_checksum               => ip_rx_ip_header_checksum,
            m_ip_source_ip                     => ip_rx_ip_source_ip,
            m_ip_dest_ip                       => ip_rx_ip_dest_ip,
            m_ip_payload_axis_tdata            => ip_rx_ip_payload_axis_tdata,
            m_ip_payload_axis_tkeep            => ip_rx_ip_payload_axis_tkeep,
            m_ip_payload_axis_tvalid           => ip_rx_ip_payload_axis_tvalid,
            m_ip_payload_axis_tready           => ip_rx_ip_payload_axis_tready,
            m_ip_payload_axis_tlast            => ip_rx_ip_payload_axis_tlast,
            m_ip_payload_axis_tuser            => ip_rx_ip_payload_axis_tuser,

            -- Status
            rx_busy                            => ip_rx_busy,
            tx_busy                            => ip_tx_busy,
            rx_error_header_early_termination  => ip_rx_error_header_early_termination,
            rx_error_payload_early_termination => ip_rx_error_payload_early_termination,
            rx_error_invalid_header            => ip_rx_error_invalid_header,
            rx_error_invalid_checksum          => ip_rx_error_invalid_checksum,
            tx_error_payload_early_termination => ip_tx_error_payload_early_termination,
            tx_error_arp_failed                => ip_tx_error_arp_failed,

            -- Configuration
            local_mac                          => local_mac,
            local_ip                           => local_ip,
            gateway_ip                         => gateway_ip,
            subnet_mask                        => subnet_mask,
            clear_arp_cache                    => clear_arp_cache
        );

    -- 
    --  UDP interface
    -- 
    udp_64_inst : entity work.udp_64
        generic map(
            CHECKSUM_GEN_ENABLE         => UDP_CHECKSUM_GEN_ENABLE,
            CHECKSUM_PAYLOAD_FIFO_DEPTH => UDP_CHECKSUM_PAYLOAD_FIFO_DEPTH,
            CHECKSUM_HEADER_FIFO_DEPTH  => UDP_CHECKSUM_HEADER_FIFO_DEPTH
        )
        port map
        (
            clk                                => clk,
            rst                                => rst,

            -- IP frame input
            s_ip_hdr_valid                     => udp_rx_ip_hdr_valid,
            s_ip_hdr_ready                     => udp_rx_ip_hdr_ready,
            s_ip_eth_dest_mac                  => udp_rx_ip_eth_dest_mac,
            s_ip_eth_src_mac                   => udp_rx_ip_eth_src_mac,
            s_ip_eth_type                      => udp_rx_ip_eth_type,
            s_ip_version                       => udp_rx_ip_version,
            s_ip_ihl                           => udp_rx_ip_ihl,
            s_ip_dscp                          => udp_rx_ip_dscp,
            s_ip_ecn                           => udp_rx_ip_ecn,
            s_ip_length                        => udp_rx_ip_length,
            s_ip_identification                => udp_rx_ip_identification,
            s_ip_flags                         => udp_rx_ip_flags,
            s_ip_fragment_offset               => udp_rx_ip_fragment_offset,
            s_ip_ttl                           => udp_rx_ip_ttl,
            s_ip_protocol                      => udp_rx_ip_protocol,
            s_ip_header_checksum               => udp_rx_ip_header_checksum,
            s_ip_source_ip                     => udp_rx_ip_source_ip,
            s_ip_dest_ip                       => udp_rx_ip_dest_ip,
            s_ip_payload_axis_tdata            => udp_rx_ip_payload_axis_tdata,
            s_ip_payload_axis_tkeep            => udp_rx_ip_payload_axis_tkeep,
            s_ip_payload_axis_tvalid           => udp_rx_ip_payload_axis_tvalid,
            s_ip_payload_axis_tready           => udp_rx_ip_payload_axis_tready,
            s_ip_payload_axis_tlast            => udp_rx_ip_payload_axis_tlast,
            s_ip_payload_axis_tuser            => udp_rx_ip_payload_axis_tuser,

            -- IP frame output
            m_ip_hdr_valid                     => udp_tx_ip_hdr_valid,
            m_ip_hdr_ready                     => udp_tx_ip_hdr_ready,
            m_ip_eth_dest_mac                  => open,
            m_ip_eth_src_mac                   => open,
            m_ip_eth_type                      => open,
            m_ip_version                       => open,
            m_ip_ihl                           => open,
            m_ip_dscp                          => udp_tx_ip_dscp,
            m_ip_ecn                           => udp_tx_ip_ecn,
            m_ip_length                        => udp_tx_ip_length,
            m_ip_identification                => open,
            m_ip_flags                         => open,
            m_ip_fragment_offset               => open,
            m_ip_ttl                           => udp_tx_ip_ttl,
            m_ip_protocol                      => udp_tx_ip_protocol,
            m_ip_header_checksum               => open,
            m_ip_source_ip                     => udp_tx_ip_source_ip,
            m_ip_dest_ip                       => udp_tx_ip_dest_ip,
            m_ip_payload_axis_tdata            => udp_tx_ip_payload_axis_tdata,
            m_ip_payload_axis_tkeep            => udp_tx_ip_payload_axis_tkeep,
            m_ip_payload_axis_tvalid           => udp_tx_ip_payload_axis_tvalid,
            m_ip_payload_axis_tready           => udp_tx_ip_payload_axis_tready,
            m_ip_payload_axis_tlast            => udp_tx_ip_payload_axis_tlast,
            m_ip_payload_axis_tuser            => udp_tx_ip_payload_axis_tuser,

            -- UDP frame input
            s_udp_hdr_valid                    => s_udp_hdr_valid,
            s_udp_hdr_ready                    => s_udp_hdr_ready,
            s_udp_eth_dest_mac                 => const_0(47 downto 0),
            s_udp_eth_src_mac                  => const_0(47 downto 0),
            s_udp_eth_type                     => const_0(16 downto 0),
            s_udp_ip_version                   => const_0(4 downto 0),
            s_udp_ip_ihl                       => const_0(4 downto 0),
            s_udp_ip_dscp                      => s_udp_ip_dscp,
            s_udp_ip_ecn                       => s_udp_ip_ecn,
            s_udp_ip_identification            => const_0(16 downto 0),
            s_udp_ip_flags                     => const_0(3 downto 0),
            s_udp_ip_fragment_offset           => const_0(13 downto 0),
            s_udp_ip_ttl                       => s_udp_ip_ttl,
            s_udp_ip_header_checksum           => const_0(16 downto 0),
            s_udp_ip_source_ip                 => s_udp_ip_source_ip,
            s_udp_ip_dest_ip                   => s_udp_ip_dest_ip,
            s_udp_source_port                  => s_udp_source_port,
            s_udp_dest_port                    => s_udp_dest_port,
            s_udp_length                       => s_udp_length,
            s_udp_checksum                     => s_udp_checksum,
            s_udp_payload_axis_tdata           => s_udp_payload_axis_tdata,
            s_udp_payload_axis_tkeep           => s_udp_payload_axis_tkeep,
            s_udp_payload_axis_tvalid          => s_udp_payload_axis_tvalid,
            s_udp_payload_axis_tready          => s_udp_payload_axis_tready,
            s_udp_payload_axis_tlast           => s_udp_payload_axis_tlast,
            s_udp_payload_axis_tuser           => s_udp_payload_axis_tuser,

            -- UDP frame output
            m_udp_hdr_valid                    => m_udp_hdr_valid,
            m_udp_hdr_ready                    => m_udp_hdr_ready,
            m_udp_eth_dest_mac                 => m_udp_eth_dest_mac,
            m_udp_eth_src_mac                  => m_udp_eth_src_mac,
            m_udp_eth_type                     => m_udp_eth_type,
            m_udp_ip_version                   => m_udp_ip_version,
            m_udp_ip_ihl                       => m_udp_ip_ihl,
            m_udp_ip_dscp                      => m_udp_ip_dscp,
            m_udp_ip_ecn                       => m_udp_ip_ecn,
            m_udp_ip_length                    => m_udp_ip_length,
            m_udp_ip_identification            => m_udp_ip_identification,
            m_udp_ip_flags                     => m_udp_ip_flags,
            m_udp_ip_fragment_offset           => m_udp_ip_fragment_offset,
            m_udp_ip_ttl                       => m_udp_ip_ttl,
            m_udp_ip_protocol                  => m_udp_ip_protocol,
            m_udp_ip_header_checksum           => m_udp_ip_header_checksum,
            m_udp_ip_source_ip                 => m_udp_ip_source_ip,
            m_udp_ip_dest_ip                   => m_udp_ip_dest_ip,
            m_udp_source_port                  => m_udp_source_port,
            m_udp_dest_port                    => m_udp_dest_port,
            m_udp_length                       => m_udp_length,
            m_udp_checksum                     => m_udp_checksum,
            m_udp_payload_axis_tdata           => m_udp_payload_axis_tdata,
            m_udp_payload_axis_tkeep           => m_udp_payload_axis_tkeep,
            m_udp_payload_axis_tvalid          => m_udp_payload_axis_tvalid,
            m_udp_payload_axis_tready          => m_udp_payload_axis_tready,
            m_udp_payload_axis_tlast           => m_udp_payload_axis_tlast,
            m_udp_payload_axis_tuser           => m_udp_payload_axis_tuser,

            -- Status
            rx_busy                            => udp_rx_busy,
            tx_busy                            => udp_tx_busy,
            rx_error_header_early_termination  => udp_rx_error_header_early_termination,
            rx_error_payload_early_termination => udp_rx_error_payload_early_termination,
            tx_error_payload_early_termination => udp_tx_error_payload_early_termination
        );

end architecture;