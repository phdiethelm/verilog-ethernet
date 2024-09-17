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
-- UDP block, IP interface (64 bit datapath)
-- 

entity udp_64 is
    generic (
        CHECKSUM_GEN_ENABLE         : integer := 1;
        CHECKSUM_PAYLOAD_FIFO_DEPTH : integer := 2048;
        CHECKSUM_HEADER_FIFO_DEPTH  : integer := 8
    );
    port (
        signal clk                                : in  std_logic;
        signal rst                                : in  std_logic;

        --
        -- IP frame input
        --
        signal s_ip_hdr_valid                     : in  std_logic;
        signal s_ip_hdr_ready                     : out std_logic;
        signal s_ip_eth_dest_mac                  : in  std_logic_vector(47 downto 0);
        signal s_ip_eth_src_mac                   : in  std_logic_vector(47 downto 0);
        signal s_ip_eth_type                      : in  std_logic_vector(15 downto 0);
        signal s_ip_version                       : in  std_logic_vector(3 downto 0);
        signal s_ip_ihl                           : in  std_logic_vector(3 downto 0);
        signal s_ip_dscp                          : in  std_logic_vector(5 downto 0);
        signal s_ip_ecn                           : in  std_logic_vector(1 downto 0);
        signal s_ip_length                        : in  std_logic_vector(15 downto 0);
        signal s_ip_identification                : in  std_logic_vector(15 downto 0);
        signal s_ip_flags                         : in  std_logic_vector(2 downto 0);
        signal s_ip_fragment_offset               : in  std_logic_vector(12 downto 0);
        signal s_ip_ttl                           : in  std_logic_vector(7 downto 0);
        signal s_ip_protocol                      : in  std_logic_vector(7 downto 0);
        signal s_ip_header_checksum               : in  std_logic_vector(15 downto 0);
        signal s_ip_source_ip                     : in  std_logic_vector(31 downto 0);
        signal s_ip_dest_ip                       : in  std_logic_vector(31 downto 0);
        signal s_ip_payload_axis_tdata            : in  std_logic_vector(63 downto 0);
        signal s_ip_payload_axis_tkeep            : in  std_logic_vector(7 downto 0);
        signal s_ip_payload_axis_tvalid           : in  std_logic;
        signal s_ip_payload_axis_tready           : out std_logic;
        signal s_ip_payload_axis_tlast            : in  std_logic;
        signal s_ip_payload_axis_tuser            : in  std_logic;

        --
        -- IP frame output
        --
        signal m_ip_hdr_valid                     : out std_logic;
        signal m_ip_hdr_ready                     : in  std_logic;
        signal m_ip_eth_dest_mac                  : out std_logic_vector(47 downto 0);
        signal m_ip_eth_src_mac                   : out std_logic_vector(47 downto 0);
        signal m_ip_eth_type                      : out std_logic_vector(15 downto 0);
        signal m_ip_version                       : out std_logic_vector(3 downto 0);
        signal m_ip_ihl                           : out std_logic_vector(3 downto 0);
        signal m_ip_dscp                          : out std_logic_vector(5 downto 0);
        signal m_ip_ecn                           : out std_logic_vector(1 downto 0);
        signal m_ip_length                        : out std_logic_vector(15 downto 0);
        signal m_ip_identification                : out std_logic_vector(15 downto 0);
        signal m_ip_flags                         : out std_logic_vector(2 downto 0);
        signal m_ip_fragment_offset               : out std_logic_vector(12 downto 0);
        signal m_ip_ttl                           : out std_logic_vector(7 downto 0);
        signal m_ip_protocol                      : out std_logic_vector(7 downto 0);
        signal m_ip_header_checksum               : out std_logic_vector(15 downto 0);
        signal m_ip_source_ip                     : out std_logic_vector(31 downto 0);
        signal m_ip_dest_ip                       : out std_logic_vector(31 downto 0);
        signal m_ip_payload_axis_tdata            : out std_logic_vector(63 downto 0);
        signal m_ip_payload_axis_tkeep            : out std_logic_vector(7 downto 0);
        signal m_ip_payload_axis_tvalid           : out std_logic;
        signal m_ip_payload_axis_tready           : in  std_logic;
        signal m_ip_payload_axis_tlast            : out std_logic;
        signal m_ip_payload_axis_tuser            : out std_logic;

        --
        -- UDP frame input
        --
        signal s_udp_hdr_valid                    : in  std_logic;
        signal s_udp_hdr_ready                    : out std_logic;
        signal s_udp_eth_dest_mac                 : in  std_logic_vector(47 downto 0);
        signal s_udp_eth_src_mac                  : in  std_logic_vector(47 downto 0);
        signal s_udp_eth_type                     : in  std_logic_vector(15 downto 0);
        signal s_udp_ip_version                   : in  std_logic_vector(3 downto 0);
        signal s_udp_ip_ihl                       : in  std_logic_vector(3 downto 0);
        signal s_udp_ip_dscp                      : in  std_logic_vector(5 downto 0);
        signal s_udp_ip_ecn                       : in  std_logic_vector(1 downto 0);
        signal s_udp_ip_identification            : in  std_logic_vector(15 downto 0);
        signal s_udp_ip_flags                     : in  std_logic_vector(2 downto 0);
        signal s_udp_ip_fragment_offset           : in  std_logic_vector(12 downto 0);
        signal s_udp_ip_ttl                       : in  std_logic_vector(7 downto 0);
        signal s_udp_ip_header_checksum           : in  std_logic_vector(15 downto 0);
        signal s_udp_ip_source_ip                 : in  std_logic_vector(31 downto 0);
        signal s_udp_ip_dest_ip                   : in  std_logic_vector(31 downto 0);
        signal s_udp_source_port                  : in  std_logic_vector(15 downto 0);
        signal s_udp_dest_port                    : in  std_logic_vector(15 downto 0);
        signal s_udp_length                       : in  std_logic_vector(15 downto 0);
        signal s_udp_checksum                     : in  std_logic_vector(15 downto 0);
        signal s_udp_payload_axis_tdata           : in  std_logic_vector(63 downto 0);
        signal s_udp_payload_axis_tkeep           : in  std_logic_vector(7 downto 0);
        signal s_udp_payload_axis_tvalid          : in  std_logic;
        signal s_udp_payload_axis_tready          : out std_logic;
        signal s_udp_payload_axis_tlast           : in  std_logic;
        signal s_udp_payload_axis_tuser           : in  std_logic;

        --
        -- UDP frame output
        --
        signal m_udp_hdr_valid                    : out std_logic;
        signal m_udp_hdr_ready                    : in  std_logic;
        signal m_udp_eth_dest_mac                 : out std_logic_vector(47 downto 0);
        signal m_udp_eth_src_mac                  : out std_logic_vector(47 downto 0);
        signal m_udp_eth_type                     : out std_logic_vector(15 downto 0);
        signal m_udp_ip_version                   : out std_logic_vector(3 downto 0);
        signal m_udp_ip_ihl                       : out std_logic_vector(3 downto 0);
        signal m_udp_ip_dscp                      : out std_logic_vector(5 downto 0);
        signal m_udp_ip_ecn                       : out std_logic_vector(1 downto 0);
        signal m_udp_ip_length                    : out std_logic_vector(15 downto 0);
        signal m_udp_ip_identification            : out std_logic_vector(15 downto 0);
        signal m_udp_ip_flags                     : out std_logic_vector(2 downto 0);
        signal m_udp_ip_fragment_offset           : out std_logic_vector(12 downto 0);
        signal m_udp_ip_ttl                       : out std_logic_vector(7 downto 0);
        signal m_udp_ip_protocol                  : out std_logic_vector(7 downto 0);
        signal m_udp_ip_header_checksum           : out std_logic_vector(15 downto 0);
        signal m_udp_ip_source_ip                 : out std_logic_vector(31 downto 0);
        signal m_udp_ip_dest_ip                   : out std_logic_vector(31 downto 0);
        signal m_udp_source_port                  : out std_logic_vector(15 downto 0);
        signal m_udp_dest_port                    : out std_logic_vector(15 downto 0);
        signal m_udp_length                       : out std_logic_vector(15 downto 0);
        signal m_udp_checksum                     : out std_logic_vector(15 downto 0);
        signal m_udp_payload_axis_tdata           : out std_logic_vector(63 downto 0);
        signal m_udp_payload_axis_tkeep           : out std_logic_vector(7 downto 0);
        signal m_udp_payload_axis_tvalid          : out std_logic;
        signal m_udp_payload_axis_tready          : in  std_logic;
        signal m_udp_payload_axis_tlast           : out std_logic;
        signal m_udp_payload_axis_tuser           : out std_logic;

        --
        -- Status signals
        --
        signal rx_busy                            : out std_logic;
        signal tx_busy                            : out std_logic;
        signal rx_error_header_early_termination  : out std_logic;
        signal rx_error_payload_early_termination : out std_logic;
        signal tx_error_payload_early_terminatio  : out std_logic
    );
end entity;

architecture rtl of udp_64 is
    signal tx_udp_hdr_valid           : std_logic;
    signal tx_udp_hdr_ready           : std_logic;
    signal tx_udp_eth_dest_mac        : std_logic_vector(47 downto 0);
    signal tx_udp_eth_src_mac         : std_logic_vector(47 downto 0);
    signal tx_udp_eth_type            : std_logic_vector(15 downto 0);
    signal tx_udp_ip_version          : std_logic_vector(3 downto 0);
    signal tx_udp_ip_ihl              : std_logic_vector(3 downto 0);
    signal tx_udp_ip_dscp             : std_logic_vector(5 downto 0);
    signal tx_udp_ip_ecn              : std_logic_vector(1 downto 0);
    signal tx_udp_ip_identification   : std_logic_vector(15 downto 0);
    signal tx_udp_ip_flags            : std_logic_vector(2 downto 0);
    signal tx_udp_ip_fragment_offset  : std_logic_vector(12 downto 0);
    signal tx_udp_ip_ttl              : std_logic_vector(7 downto 0);
    signal tx_udp_ip_header_checksum  : std_logic_vector(15 downto 0);
    signal tx_udp_ip_source_ip        : std_logic_vector(31 downto 0);
    signal tx_udp_ip_dest_ip          : std_logic_vector(31 downto 0);
    signal tx_udp_source_port         : std_logic_vector(15 downto 0);
    signal tx_udp_dest_port           : std_logic_vector(15 downto 0);
    signal tx_udp_length              : std_logic_vector(15 downto 0);
    signal tx_udp_checksum            : std_logic_vector(15 downto 0);
    signal tx_udp_payload_axis_tdata  : std_logic_vector(63 downto 0);
    signal tx_udp_payload_axis_tkeep  : std_logic_vector(7 downto 0);
    signal tx_udp_payload_axis_tvalid : std_logic;
    signal tx_udp_payload_axis_tready : std_logic;
    signal tx_udp_payload_axis_tlast  : std_logic;
    signal tx_udp_payload_axis_tuser  : std_logic;
begin

    udp_ip_rx_64_inst : entity work.udp_ip_rx_64
        port map(
            clk                             => clk,
            rst                             => rst,

            -- IP frame input
            s_ip_hdr_valid                  => s_ip_hdr_valid,
            s_ip_hdr_ready                  => s_ip_hdr_ready,
            s_eth_dest_mac                  => s_ip_eth_dest_mac,
            s_eth_src_mac                   => s_ip_eth_src_mac,
            s_eth_type                      => s_ip_eth_type,
            s_ip_version                    => s_ip_version,
            s_ip_ihl                        => s_ip_ihl,
            s_ip_dscp                       => s_ip_dscp,
            s_ip_ecn                        => s_ip_ecn,
            s_ip_length                     => s_ip_length,
            s_ip_identification             => s_ip_identification,
            s_ip_flags                      => s_ip_flags,
            s_ip_fragment_offset            => s_ip_fragment_offset,
            s_ip_ttl                        => s_ip_ttl,
            s_ip_protocol                   => s_ip_protocol,
            s_ip_header_checksum            => s_ip_header_checksum,
            s_ip_source_ip                  => s_ip_source_ip,
            s_ip_dest_ip                    => s_ip_dest_ip,
            s_ip_payload_axis_tdata         => s_ip_payload_axis_tdata,
            s_ip_payload_axis_tkeep         => s_ip_payload_axis_tkeep,
            s_ip_payload_axis_tvalid        => s_ip_payload_axis_tvalid,
            s_ip_payload_axis_tready        => s_ip_payload_axis_tready,
            s_ip_payload_axis_tlast         => s_ip_payload_axis_tlast,
            s_ip_payload_axis_tuser         => s_ip_payload_axis_tuser,

            -- UDP frame output
            m_udp_hdr_valid                 => m_udp_hdr_valid,
            m_udp_hdr_ready                 => m_udp_hdr_ready,
            m_eth_dest_mac                  => m_udp_eth_dest_mac,
            m_eth_src_mac                   => m_udp_eth_src_mac,
            m_eth_type                      => m_udp_eth_type,
            m_ip_version                    => m_udp_ip_version,
            m_ip_ihl                        => m_udp_ip_ihl,
            m_ip_dscp                       => m_udp_ip_dscp,
            m_ip_ecn                        => m_udp_ip_ecn,
            m_ip_length                     => m_udp_ip_length,
            m_ip_identification             => m_udp_ip_identification,
            m_ip_flags                      => m_udp_ip_flags,
            m_ip_fragment_offset            => m_udp_ip_fragment_offset,
            m_ip_ttl                        => m_udp_ip_ttl,
            m_ip_protocol                   => m_udp_ip_protocol,
            m_ip_header_checksum            => m_udp_ip_header_checksum,
            m_ip_source_ip                  => m_udp_ip_source_ip,
            m_ip_dest_ip                    => m_udp_ip_dest_ip,
            m_udp_source_port               => m_udp_source_port,
            m_udp_dest_port                 => m_udp_dest_port,
            m_udp_length                    => m_udp_length,
            m_udp_checksum                  => m_udp_checksum,
            m_udp_payload_axis_tdata        => m_udp_payload_axis_tdata,
            m_udp_payload_axis_tkeep        => m_udp_payload_axis_tkeep,
            m_udp_payload_axis_tvalid       => m_udp_payload_axis_tvalid,
            m_udp_payload_axis_tready       => m_udp_payload_axis_tready,
            m_udp_payload_axis_tlast        => m_udp_payload_axis_tlast,
            m_udp_payload_axis_tuser        => m_udp_payload_axis_tuser,

            -- Status signals
            busy                            => rx_busy,
            error_header_early_termination  => rx_error_header_early_termination,
            error_payload_early_termination => rx_error_payload_early_termination
        );

    gc_on : if CHECKSUM_GEN_ENABLE = 1 generate

        udp_checksum_gen_64_inst : entity work.udp_checksum_gen_64
            generic map(
                PAYLOAD_FIFO_DEPTH => CHECKSUM_PAYLOAD_FIFO_DEPTH,
                HEADER_FIFO_DEPTH  => CHECKSUM_HEADER_FIFO_DEPTH
            )
            port map(
                clk                       => clk,
                rst                       => rst,

                -- UDP frame input
                s_udp_hdr_valid           => s_udp_hdr_valid,
                s_udp_hdr_ready           => s_udp_hdr_ready,
                s_eth_dest_mac            => s_udp_eth_dest_mac,
                s_eth_src_mac             => s_udp_eth_src_mac,
                s_eth_type                => s_udp_eth_type,
                s_ip_version              => s_udp_ip_version,
                s_ip_ihl                  => s_udp_ip_ihl,
                s_ip_dscp                 => s_udp_ip_dscp,
                s_ip_ecn                  => s_udp_ip_ecn,
                s_ip_identification       => s_udp_ip_identification,
                s_ip_flags                => s_udp_ip_flags,
                s_ip_fragment_offset      => s_udp_ip_fragment_offset,
                s_ip_ttl                  => s_udp_ip_ttl,
                s_ip_header_checksum      => s_udp_ip_header_checksum,
                s_ip_source_ip            => s_udp_ip_source_ip,
                s_ip_dest_ip              => s_udp_ip_dest_ip,
                s_udp_source_port         => s_udp_source_port,
                s_udp_dest_port           => s_udp_dest_port,
                s_udp_payload_axis_tdata  => s_udp_payload_axis_tdata,
                s_udp_payload_axis_tkeep  => s_udp_payload_axis_tkeep,
                s_udp_payload_axis_tvalid => s_udp_payload_axis_tvalid,
                s_udp_payload_axis_tready => s_udp_payload_axis_tready,
                s_udp_payload_axis_tlast  => s_udp_payload_axis_tlast,
                s_udp_payload_axis_tuser  => s_udp_payload_axis_tuser,

                -- UDP frame output
                m_udp_hdr_valid           => tx_udp_hdr_valid,
                m_udp_hdr_ready           => tx_udp_hdr_ready,
                m_eth_dest_mac            => tx_udp_eth_dest_mac,
                m_eth_src_mac             => tx_udp_eth_src_mac,
                m_eth_type                => tx_udp_eth_type,
                m_ip_version              => tx_udp_ip_version,
                m_ip_ihl                  => tx_udp_ip_ihl,
                m_ip_dscp                 => tx_udp_ip_dscp,
                m_ip_ecn                  => tx_udp_ip_ecn,
                m_ip_length               => open,
                m_ip_identification       => tx_udp_ip_identification,
                m_ip_flags                => tx_udp_ip_flags,
                m_ip_fragment_offset      => tx_udp_ip_fragment_offset,
                m_ip_ttl                  => tx_udp_ip_ttl,
                m_ip_header_checksum      => tx_udp_ip_header_checksum,
                m_ip_source_ip            => tx_udp_ip_source_ip,
                m_ip_dest_ip              => tx_udp_ip_dest_ip,
                m_udp_source_port         => tx_udp_source_port,
                m_udp_dest_port           => tx_udp_dest_port,
                m_udp_length              => tx_udp_length,
                m_udp_checksum            => tx_udp_checksum,
                m_udp_payload_axis_tdata  => tx_udp_payload_axis_tdata,
                m_udp_payload_axis_tkeep  => tx_udp_payload_axis_tkeep,
                m_udp_payload_axis_tvalid => tx_udp_payload_axis_tvalid,
                m_udp_payload_axis_tready => tx_udp_payload_axis_tready,
                m_udp_payload_axis_tlast  => tx_udp_payload_axis_tlast,
                m_udp_payload_axis_tuser  => tx_udp_payload_axis_tuser,
                -- Status signals
                busy                      => open
            );

    end generate;

    gc_off : if CHECKSUM_GEN_ENABLE /= 1 generate
        tx_udp_hdr_valid           <= s_udp_hdr_valid;
        s_udp_hdr_ready            <= tx_udp_hdr_ready;
        tx_udp_eth_dest_mac        <= s_udp_eth_dest_mac;
        tx_udp_eth_src_mac         <= s_udp_eth_src_mac;
        tx_udp_eth_type            <= s_udp_eth_type;
        tx_udp_ip_version          <= s_udp_ip_version;
        tx_udp_ip_ihl              <= s_udp_ip_ihl;
        tx_udp_ip_dscp             <= s_udp_ip_dscp;
        tx_udp_ip_ecn              <= s_udp_ip_ecn;
        tx_udp_ip_identification   <= s_udp_ip_identification;
        tx_udp_ip_flags            <= s_udp_ip_flags;
        tx_udp_ip_fragment_offset  <= s_udp_ip_fragment_offset;
        tx_udp_ip_ttl              <= s_udp_ip_ttl;
        tx_udp_ip_header_checksum  <= s_udp_ip_header_checksum;
        tx_udp_ip_source_ip        <= s_udp_ip_source_ip;
        tx_udp_ip_dest_ip          <= s_udp_ip_dest_ip;
        tx_udp_source_port         <= s_udp_source_port;
        tx_udp_dest_port           <= s_udp_dest_port;
        tx_udp_length              <= s_udp_length;
        tx_udp_checksum            <= s_udp_checksum;
        tx_udp_payload_axis_tdata  <= s_udp_payload_axis_tdata;
        tx_udp_payload_axis_tkeep  <= s_udp_payload_axis_tkeep;
        tx_udp_payload_axis_tvalid <= s_udp_payload_axis_tvalid;
        s_udp_payload_axis_tready  <= tx_udp_payload_axis_tready;
        tx_udp_payload_axis_tlast  <= s_udp_payload_axis_tlast;
        tx_udp_payload_axis_tuser  <= s_udp_payload_axis_tuser;

    end generate;

    udp_ip_tx_64_inst : entity work.udp_ip_tx_64
        port map(
            clk                             => clk,
            rst                             => rst,
            -- UDP frame input
            s_udp_hdr_valid                 => tx_udp_hdr_valid,
            s_udp_hdr_ready                 => tx_udp_hdr_ready,
            s_eth_dest_mac                  => tx_udp_eth_dest_mac,
            s_eth_src_mac                   => tx_udp_eth_src_mac,
            s_eth_type                      => tx_udp_eth_type,
            s_ip_version                    => tx_udp_ip_version,
            s_ip_ihl                        => tx_udp_ip_ihl,
            s_ip_dscp                       => tx_udp_ip_dscp,
            s_ip_ecn                        => tx_udp_ip_ecn,
            s_ip_identification             => tx_udp_ip_identification,
            s_ip_flags                      => tx_udp_ip_flags,
            s_ip_fragment_offset            => tx_udp_ip_fragment_offset,
            s_ip_ttl                        => tx_udp_ip_ttl,
            s_ip_protocol                   => x"11",
            s_ip_header_checksum            => tx_udp_ip_header_checksum,
            s_ip_source_ip                  => tx_udp_ip_source_ip,
            s_ip_dest_ip                    => tx_udp_ip_dest_ip,
            s_udp_source_port               => tx_udp_source_port,
            s_udp_dest_port                 => tx_udp_dest_port,
            s_udp_length                    => tx_udp_length,
            s_udp_checksum                  => tx_udp_checksum,
            s_udp_payload_axis_tdata        => tx_udp_payload_axis_tdata,
            s_udp_payload_axis_tkeep        => tx_udp_payload_axis_tkeep,
            s_udp_payload_axis_tvalid       => tx_udp_payload_axis_tvalid,
            s_udp_payload_axis_tready       => tx_udp_payload_axis_tready,
            s_udp_payload_axis_tlast        => tx_udp_payload_axis_tlast,
            s_udp_payload_axis_tuser        => tx_udp_payload_axis_tuser,
            -- IP frame output
            m_ip_hdr_valid                  => m_ip_hdr_valid,
            m_ip_hdr_ready                  => m_ip_hdr_ready,
            m_eth_dest_mac                  => m_ip_eth_dest_mac,
            m_eth_src_mac                   => m_ip_eth_src_mac,
            m_eth_type                      => m_ip_eth_type,
            m_ip_version                    => m_ip_version,
            m_ip_ihl                        => m_ip_ihl,
            m_ip_dscp                       => m_ip_dscp,
            m_ip_ecn                        => m_ip_ecn,
            m_ip_length                     => m_ip_length,
            m_ip_identification             => m_ip_identification,
            m_ip_flags                      => m_ip_flags,
            m_ip_fragment_offset            => m_ip_fragment_offset,
            m_ip_ttl                        => m_ip_ttl,
            m_ip_protocol                   => m_ip_protocol,
            m_ip_header_checksum            => m_ip_header_checksum,
            m_ip_source_ip                  => m_ip_source_ip,
            m_ip_dest_ip                    => m_ip_dest_ip,
            m_ip_payload_axis_tdata         => m_ip_payload_axis_tdata,
            m_ip_payload_axis_tkeep         => m_ip_payload_axis_tkeep,
            m_ip_payload_axis_tvalid        => m_ip_payload_axis_tvalid,
            m_ip_payload_axis_tready        => m_ip_payload_axis_tready,
            m_ip_payload_axis_tlast         => m_ip_payload_axis_tlast,
            m_ip_payload_axis_tuser         => m_ip_payload_axis_tuser,
            -- Status signals
            busy                            => tx_busy,
            error_payload_early_termination => tx_error_payload_early_termination
        );

end architecture;