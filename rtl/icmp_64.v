/*

Copyright (c) 2024 Philipp Diethelm
Copyright (c) 2014-2018 Alex Forencich

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*/

// Language: Verilog 2001

`resetall
`timescale 1ns / 1ps
`default_nettype none

/*
 * ICMP block, IP interface (64 bit datapath)
 */
module icmp_64 #
(
    parameter CHECKSUM_GEN_ENABLE = 1,
    parameter CHECKSUM_PAYLOAD_FIFO_DEPTH = 2048,
    parameter CHECKSUM_HEADER_FIFO_DEPTH = 8
)
(
    input  wire        clk,
    input  wire        rst,
    
    /*
     * IP frame input
     */
    input  wire        s_ip_hdr_valid,
    output wire        s_ip_hdr_ready,
    input  wire [47:0] s_ip_eth_dest_mac,
    input  wire [47:0] s_ip_eth_src_mac,
    input  wire [15:0] s_ip_eth_type,
    input  wire [3:0]  s_ip_version,
    input  wire [3:0]  s_ip_ihl,
    input  wire [5:0]  s_ip_dscp,
    input  wire [1:0]  s_ip_ecn,
    input  wire [15:0] s_ip_length,
    input  wire [15:0] s_ip_identification,
    input  wire [2:0]  s_ip_flags,
    input  wire [12:0] s_ip_fragment_offset,
    input  wire [7:0]  s_ip_ttl,
    input  wire [7:0]  s_ip_protocol,
    input  wire [15:0] s_ip_header_checksum,
    input  wire [31:0] s_ip_source_ip,
    input  wire [31:0] s_ip_dest_ip,
    input  wire [63:0] s_ip_payload_axis_tdata,
    input  wire [7:0]  s_ip_payload_axis_tkeep,
    input  wire        s_ip_payload_axis_tvalid,
    output wire        s_ip_payload_axis_tready,
    input  wire        s_ip_payload_axis_tlast,
    input  wire        s_ip_payload_axis_tuser,
    
    /*
     * IP frame output
     */
    output wire        m_ip_hdr_valid,
    input  wire        m_ip_hdr_ready,
    output wire [47:0] m_ip_eth_dest_mac,
    output wire [47:0] m_ip_eth_src_mac,
    output wire [15:0] m_ip_eth_type,
    output wire [3:0]  m_ip_version,
    output wire [3:0]  m_ip_ihl,
    output wire [5:0]  m_ip_dscp,
    output wire [1:0]  m_ip_ecn,
    output wire [15:0] m_ip_length,
    output wire [15:0] m_ip_identification,
    output wire [2:0]  m_ip_flags,
    output wire [12:0] m_ip_fragment_offset,
    output wire [7:0]  m_ip_ttl,
    output wire [7:0]  m_ip_protocol,
    output wire [15:0] m_ip_header_checksum,
    output wire [31:0] m_ip_source_ip,
    output wire [31:0] m_ip_dest_ip,
    output wire [63:0] m_ip_payload_axis_tdata,
    output wire [7:0]  m_ip_payload_axis_tkeep,
    output wire        m_ip_payload_axis_tvalid,
    input  wire        m_ip_payload_axis_tready,
    output wire        m_ip_payload_axis_tlast,
    output wire        m_ip_payload_axis_tuser,

    /*
     * Status signals
     */
    output wire        rx_busy,
    output wire        tx_busy,
    output wire        rx_error_header_early_termination,
    output wire        rx_error_payload_early_termination,
    output wire        tx_error_payload_early_termination
);

wire        cs_tx_icmp_hdr_valid;
wire        cs_tx_icmp_hdr_ready;
wire [47:0] cs_tx_icmp_eth_dest_mac;
wire [47:0] cs_tx_icmp_eth_src_mac;
wire [15:0] cs_tx_icmp_eth_type;
wire [3:0]  cs_tx_icmp_ip_version;
wire [3:0]  cs_tx_icmp_ip_ihl;
wire [5:0]  cs_tx_icmp_ip_dscp;
wire [1:0]  cs_tx_icmp_ip_ecn;
wire [15:0] cs_tx_icmp_ip_identification;
wire [2:0]  cs_tx_icmp_ip_flags;
wire [12:0] cs_tx_icmp_ip_fragment_offset;
wire [7:0]  cs_tx_icmp_ip_ttl;
wire [15:0] cs_tx_icmp_ip_header_checksum;
wire [31:0] cs_tx_icmp_ip_source_ip;
wire [31:0] cs_tx_icmp_ip_dest_ip;
wire [7:0]  cs_tx_icmp_type;
wire [7:0]  cs_tx_icmp_code;
wire [15:0] cs_tx_icmp_checksum;
wire [31:0] cs_tx_icmp_header;
wire [15:0] cs_tx_icmp_length;
wire [63:0] cs_tx_icmp_payload_axis_tdata;
wire [7:0]  cs_tx_icmp_payload_axis_tkeep;
wire        cs_tx_icmp_payload_axis_tvalid;
wire        cs_tx_icmp_payload_axis_tready;
wire        cs_tx_icmp_payload_axis_tlast;
wire        cs_tx_icmp_payload_axis_tuser;

/*
 * ICMP frame input
 */
wire        tx_icmp_hdr_valid;
wire        tx_icmp_hdr_ready;
wire [47:0] tx_icmp_eth_dest_mac;
wire [47:0] tx_icmp_eth_src_mac;
wire [15:0] tx_icmp_eth_type;
wire [3:0]  tx_icmp_ip_version;
wire [3:0]  tx_icmp_ip_ihl;
wire [5:0]  tx_icmp_ip_dscp;
wire [1:0]  tx_icmp_ip_ecn;
wire [15:0] tx_icmp_ip_identification;
wire [2:0]  tx_icmp_ip_flags;
wire [12:0] tx_icmp_ip_fragment_offset;
wire [7:0]  tx_icmp_ip_ttl;
wire [15:0] tx_icmp_ip_header_checksum;
wire [31:0] tx_icmp_ip_source_ip;
wire [31:0] tx_icmp_ip_dest_ip;
wire [7:0]  tx_icmp_type;
wire [7:0]  tx_icmp_code;
wire [15:0] tx_icmp_checksum;
wire [31:0] tx_icmp_header;
wire [15:0] tx_icmp_length;
wire [63:0] tx_icmp_payload_axis_tdata;
wire [7:0]  tx_icmp_payload_axis_tkeep;
wire        tx_icmp_payload_axis_tvalid;
wire        tx_icmp_payload_axis_tready;
wire        tx_icmp_payload_axis_tlast;
wire        tx_icmp_payload_axis_tuser;
    
/*
 * ICMP frame output
 */
wire        rx_icmp_hdr_valid;
wire        rx_icmp_hdr_ready;
wire [47:0] rx_icmp_eth_dest_mac;
wire [47:0] rx_icmp_eth_src_mac;
wire [15:0] rx_icmp_eth_type;
wire [3:0]  rx_icmp_ip_version;
wire [3:0]  rx_icmp_ip_ihl;
wire [5:0]  rx_icmp_ip_dscp;
wire [1:0]  rx_icmp_ip_ecn;
wire [15:0] rx_icmp_ip_length;
wire [15:0] rx_icmp_ip_identification;
wire [2:0]  rx_icmp_ip_flags;
wire [12:0] rx_icmp_ip_fragment_offset;
wire [7:0]  rx_icmp_ip_ttl;
wire [7:0]  rx_icmp_ip_protocol;
wire [15:0] rx_icmp_ip_header_checksum;
wire [31:0] rx_icmp_ip_source_ip;
wire [31:0] rx_icmp_ip_dest_ip;
wire [7:0]  rx_icmp_type;
wire [7:0]  rx_icmp_code;
wire [15:0] rx_icmp_checksum;
wire [31:0] rx_icmp_header;
wire [15:0] rx_icmp_length;
wire [63:0] rx_icmp_payload_axis_tdata;
wire [7:0]  rx_icmp_payload_axis_tkeep;
wire        rx_icmp_payload_axis_tvalid;
wire        rx_icmp_payload_axis_tready;
wire        rx_icmp_payload_axis_tlast;
wire        rx_icmp_payload_axis_tuser;

icmp_ip_rx_64
icmp_ip_rx_64_inst (
    .clk(clk),
    .rst(rst),
    // IP frame input
    .s_ip_hdr_valid(s_ip_hdr_valid),
    .s_ip_hdr_ready(s_ip_hdr_ready),
    .s_eth_dest_mac(s_ip_eth_dest_mac),
    .s_eth_src_mac(s_ip_eth_src_mac),
    .s_eth_type(s_ip_eth_type),
    .s_ip_version(s_ip_version),
    .s_ip_ihl(s_ip_ihl),
    .s_ip_dscp(s_ip_dscp),
    .s_ip_ecn(s_ip_ecn),
    .s_ip_length(s_ip_length),
    .s_ip_identification(s_ip_identification),
    .s_ip_flags(s_ip_flags),
    .s_ip_fragment_offset(s_ip_fragment_offset),
    .s_ip_ttl(s_ip_ttl),
    .s_ip_protocol(s_ip_protocol),
    .s_ip_header_checksum(s_ip_header_checksum),
    .s_ip_source_ip(s_ip_source_ip),
    .s_ip_dest_ip(s_ip_dest_ip),
    .s_ip_payload_axis_tdata(s_ip_payload_axis_tdata),
    .s_ip_payload_axis_tkeep(s_ip_payload_axis_tkeep),
    .s_ip_payload_axis_tvalid(s_ip_payload_axis_tvalid),
    .s_ip_payload_axis_tready(s_ip_payload_axis_tready),
    .s_ip_payload_axis_tlast(s_ip_payload_axis_tlast),
    .s_ip_payload_axis_tuser(s_ip_payload_axis_tuser),
    // ICMP frame output
    .m_icmp_hdr_valid(rx_icmp_hdr_valid),
    .m_icmp_hdr_ready(rx_icmp_hdr_ready),
    .m_eth_dest_mac(rx_icmp_eth_dest_mac),
    .m_eth_src_mac(rx_icmp_eth_src_mac),
    .m_eth_type(rx_icmp_eth_type),
    .m_ip_version(rx_icmp_ip_version),
    .m_ip_ihl(rx_icmp_ip_ihl),
    .m_ip_dscp(rx_icmp_ip_dscp),
    .m_ip_ecn(rx_icmp_ip_ecn),
    .m_ip_length(rx_icmp_ip_length),
    .m_ip_identification(rx_icmp_ip_identification),
    .m_ip_flags(rx_icmp_ip_flags),
    .m_ip_fragment_offset(rx_icmp_ip_fragment_offset),
    .m_ip_ttl(rx_icmp_ip_ttl),
    .m_ip_protocol(rx_icmp_ip_protocol),
    .m_ip_header_checksum(rx_icmp_ip_header_checksum),
    .m_ip_source_ip(rx_icmp_ip_source_ip),
    .m_ip_dest_ip(rx_icmp_ip_dest_ip),
    .m_icmp_type(rx_icmp_type),
    .m_icmp_code(rx_icmp_code),
    .m_icmp_checksum(rx_icmp_checksum),
    .m_icmp_header(rx_icmp_header),
    .m_icmp_length(rx_icmp_length),
    .m_icmp_payload_axis_tdata(rx_icmp_payload_axis_tdata),
    .m_icmp_payload_axis_tkeep(rx_icmp_payload_axis_tkeep),
    .m_icmp_payload_axis_tvalid(rx_icmp_payload_axis_tvalid),
    .m_icmp_payload_axis_tready(rx_icmp_payload_axis_tready),
    .m_icmp_payload_axis_tlast(rx_icmp_payload_axis_tlast),
    .m_icmp_payload_axis_tuser(rx_icmp_payload_axis_tuser),
    // Status signals
    .busy(rx_busy),
    .error_header_early_termination(rx_error_header_early_termination),
    .error_payload_early_termination(rx_error_payload_early_termination)
);

generate

if (CHECKSUM_GEN_ENABLE) begin

    icmp_checksum_gen_64 #(
        .PAYLOAD_FIFO_DEPTH(CHECKSUM_PAYLOAD_FIFO_DEPTH),
        .HEADER_FIFO_DEPTH(CHECKSUM_HEADER_FIFO_DEPTH)
    )
    icmp_checksum_gen_64_inst (
        .clk(clk),
        .rst(rst),
        // ICMP frame input
        .s_icmp_hdr_valid(tx_icmp_hdr_valid),
        .s_icmp_hdr_ready(tx_icmp_hdr_ready),
        .s_eth_dest_mac(tx_icmp_eth_dest_mac),
        .s_eth_src_mac(tx_icmp_eth_src_mac),
        .s_eth_type(tx_icmp_eth_type),
        .s_ip_version(tx_icmp_ip_version),
        .s_ip_ihl(tx_icmp_ip_ihl),
        .s_ip_dscp(tx_icmp_ip_dscp),
        .s_ip_ecn(tx_icmp_ip_ecn),
        .s_ip_identification(tx_icmp_ip_identification),
        .s_ip_flags(tx_icmp_ip_flags),
        .s_ip_fragment_offset(tx_icmp_ip_fragment_offset),
        .s_ip_ttl(tx_icmp_ip_ttl),
        .s_ip_header_checksum(tx_icmp_ip_header_checksum),
        .s_ip_source_ip(tx_icmp_ip_source_ip),
        .s_ip_dest_ip(tx_icmp_ip_dest_ip),
        .s_icmp_type(tx_icmp_type),
        .s_icmp_code(tx_icmp_code),
        .s_icmp_header(tx_icmp_header),
        .s_icmp_length(tx_icmp_length),
        .s_icmp_payload_axis_tdata(tx_icmp_payload_axis_tdata),
        .s_icmp_payload_axis_tkeep(tx_icmp_payload_axis_tkeep),
        .s_icmp_payload_axis_tvalid(tx_icmp_payload_axis_tvalid),
        .s_icmp_payload_axis_tready(tx_icmp_payload_axis_tready),
        .s_icmp_payload_axis_tlast(tx_icmp_payload_axis_tlast),
        .s_icmp_payload_axis_tuser(tx_icmp_payload_axis_tuser),
        // ICMP frame output
        .m_icmp_hdr_valid(cs_tx_icmp_hdr_valid),
        .m_icmp_hdr_ready(cs_tx_icmp_hdr_ready),
        .m_eth_dest_mac(cs_tx_icmp_eth_dest_mac),
        .m_eth_src_mac(cs_tx_icmp_eth_src_mac),
        .m_eth_type(cs_tx_icmp_eth_type),
        .m_ip_version(cs_tx_icmp_ip_version),
        .m_ip_ihl(cs_tx_icmp_ip_ihl),
        .m_ip_dscp(cs_tx_icmp_ip_dscp),
        .m_ip_ecn(cs_tx_icmp_ip_ecn),
        .m_ip_length(),
        .m_ip_identification(cs_tx_icmp_ip_identification),
        .m_ip_flags(cs_tx_icmp_ip_flags),
        .m_ip_fragment_offset(cs_tx_icmp_ip_fragment_offset),
        .m_ip_ttl(cs_tx_icmp_ip_ttl),
        .m_ip_header_checksum(cs_tx_icmp_ip_header_checksum),
        .m_ip_source_ip(cs_tx_icmp_ip_source_ip),
        .m_ip_dest_ip(cs_tx_icmp_ip_dest_ip),
        .m_icmp_type(cs_tx_icmp_type),
        .m_icmp_code(cs_tx_icmp_code),
        .m_icmp_checksum(cs_tx_icmp_checksum),
        .m_icmp_header(cs_tx_icmp_header),
        .m_icmp_length(cs_tx_icmp_length),
        .m_icmp_payload_axis_tdata(cs_tx_icmp_payload_axis_tdata),
        .m_icmp_payload_axis_tkeep(cs_tx_icmp_payload_axis_tkeep),
        .m_icmp_payload_axis_tvalid(cs_tx_icmp_payload_axis_tvalid),
        .m_icmp_payload_axis_tready(cs_tx_icmp_payload_axis_tready),
        .m_icmp_payload_axis_tlast(cs_tx_icmp_payload_axis_tlast),
        .m_icmp_payload_axis_tuser(cs_tx_icmp_payload_axis_tuser),
        // Status signals
        .busy()
    );

end else begin

    assign cs_tx_icmp_hdr_valid = tx_icmp_hdr_valid;
    assign tx_icmp_hdr_ready = cs_tx_icmp_hdr_ready;
    assign cs_tx_icmp_eth_dest_mac = tx_icmp_eth_dest_mac;
    assign cs_tx_icmp_eth_src_mac = tx_icmp_eth_src_mac;
    assign cs_tx_icmp_eth_type = tx_icmp_eth_type;
    assign cs_tx_icmp_ip_version = tx_icmp_ip_version;
    assign cs_tx_icmp_ip_ihl = tx_icmp_ip_ihl;
    assign cs_tx_icmp_ip_dscp = tx_icmp_ip_dscp;
    assign cs_tx_icmp_ip_ecn = tx_icmp_ip_ecn;
    assign cs_tx_icmp_ip_identification = tx_icmp_ip_identification;
    assign cs_tx_icmp_ip_flags = tx_icmp_ip_flags;
    assign cs_tx_icmp_ip_fragment_offset = tx_icmp_ip_fragment_offset;
    assign cs_tx_icmp_ip_ttl = tx_icmp_ip_ttl;
    assign cs_tx_icmp_ip_header_checksum = tx_icmp_ip_header_checksum;
    assign cs_tx_icmp_ip_source_ip = tx_icmp_ip_source_ip;
    assign cs_tx_icmp_ip_dest_ip = tx_icmp_ip_dest_ip;
    assign cs_tx_icmp_type = tx_icmp_type;
    assign cs_tx_icmp_code = tx_icmp_code;
    assign cs_tx_icmp_checksum = tx_icmp_checksum;
    assign cs_tx_icmp_header = tx_icmp_header;
    assign cs_tx_icmp_length = tx_icmp_length;
    assign cs_tx_icmp_payload_axis_tdata = tx_icmp_payload_axis_tdata;
    assign cs_tx_icmp_payload_axis_tkeep = tx_icmp_payload_axis_tkeep;
    assign cs_tx_icmp_payload_axis_tvalid = tx_icmp_payload_axis_tvalid;
    assign tx_icmp_payload_axis_tready = cs_tx_icmp_payload_axis_tready;
    assign cs_tx_icmp_payload_axis_tlast = tx_icmp_payload_axis_tlast;
    assign cs_tx_icmp_payload_axis_tuser = tx_icmp_payload_axis_tuser;

end

endgenerate

icmp_ip_tx_64
icmp_ip_tx_64_inst (
    .clk(clk),
    .rst(rst),
    // UDP frame input
    .s_icmp_hdr_valid(cs_tx_icmp_hdr_valid),
    .s_icmp_hdr_ready(cs_tx_icmp_hdr_ready),
    .s_eth_dest_mac(cs_tx_icmp_eth_dest_mac),
    .s_eth_src_mac(cs_tx_icmp_eth_src_mac),
    .s_eth_type(cs_tx_icmp_eth_type),
    .s_ip_version(cs_tx_icmp_ip_version),
    .s_ip_ihl(cs_tx_icmp_ip_ihl),
    .s_ip_dscp(cs_tx_icmp_ip_dscp),
    .s_ip_ecn(cs_tx_icmp_ip_ecn),
    .s_ip_identification(cs_tx_icmp_ip_identification),
    .s_ip_flags(cs_tx_icmp_ip_flags),
    .s_ip_fragment_offset(cs_tx_icmp_ip_fragment_offset),
    .s_ip_ttl(cs_tx_icmp_ip_ttl),
    .s_ip_protocol(8'h01),
    .s_ip_header_checksum(cs_tx_icmp_ip_header_checksum),
    .s_ip_source_ip(cs_tx_icmp_ip_source_ip),
    .s_ip_dest_ip(cs_tx_icmp_ip_dest_ip),
    .s_icmp_type(cs_tx_icmp_type),
    .s_icmp_code(cs_tx_icmp_code),
    .s_icmp_checksum(cs_tx_icmp_checksum),
    .s_icmp_header(cs_tx_icmp_header),
    .s_icmp_length(cs_tx_icmp_length),
    .s_icmp_payload_axis_tdata(cs_tx_icmp_payload_axis_tdata),
    .s_icmp_payload_axis_tkeep(cs_tx_icmp_payload_axis_tkeep),
    .s_icmp_payload_axis_tvalid(cs_tx_icmp_payload_axis_tvalid),
    .s_icmp_payload_axis_tready(cs_tx_icmp_payload_axis_tready),
    .s_icmp_payload_axis_tlast(cs_tx_icmp_payload_axis_tlast),
    .s_icmp_payload_axis_tuser(cs_tx_icmp_payload_axis_tuser),
    // IP frame output
    .m_ip_hdr_valid(m_ip_hdr_valid),
    .m_ip_hdr_ready(m_ip_hdr_ready),
    .m_eth_dest_mac(m_ip_eth_dest_mac),
    .m_eth_src_mac(m_ip_eth_src_mac),
    .m_eth_type(m_ip_eth_type),
    .m_ip_version(m_ip_version),
    .m_ip_ihl(m_ip_ihl),
    .m_ip_dscp(m_ip_dscp),
    .m_ip_ecn(m_ip_ecn),
    .m_ip_length(m_ip_length),
    .m_ip_identification(m_ip_identification),
    .m_ip_flags(m_ip_flags),
    .m_ip_fragment_offset(m_ip_fragment_offset),
    .m_ip_ttl(m_ip_ttl),
    .m_ip_protocol(m_ip_protocol),
    .m_ip_header_checksum(m_ip_header_checksum),
    .m_ip_source_ip(m_ip_source_ip),
    .m_ip_dest_ip(m_ip_dest_ip),
    .m_ip_payload_axis_tdata(m_ip_payload_axis_tdata),
    .m_ip_payload_axis_tkeep(m_ip_payload_axis_tkeep),
    .m_ip_payload_axis_tvalid(m_ip_payload_axis_tvalid),
    .m_ip_payload_axis_tready(m_ip_payload_axis_tready),
    .m_ip_payload_axis_tlast(m_ip_payload_axis_tlast),
    .m_ip_payload_axis_tuser(m_ip_payload_axis_tuser),
    // Status signals
    .busy(tx_busy),
    .error_payload_early_termination(tx_error_payload_early_termination)
);

icmp_echo_64
icmp_echo_64_inst (
    .clk(clk),
    .rst(rst),

    // ICMP frame input
    .tx_icmp_hdr_valid(tx_icmp_hdr_valid),
    .tx_icmp_hdr_ready(tx_icmp_hdr_ready),
    .tx_icmp_eth_dest_mac(tx_icmp_eth_dest_mac),
    .tx_icmp_eth_src_mac(tx_icmp_eth_src_mac),
    .tx_icmp_eth_type(tx_icmp_eth_type),
    .tx_icmp_ip_version(tx_icmp_ip_version),
    .tx_icmp_ip_ihl(tx_icmp_ip_ihl),
    .tx_icmp_ip_dscp(tx_icmp_ip_dscp),
    .tx_icmp_ip_ecn(tx_icmp_ip_ecn),
    .tx_icmp_ip_identification(tx_icmp_ip_identification),
    .tx_icmp_ip_flags(tx_icmp_ip_flags),
    .tx_icmp_ip_fragment_offset(tx_icmp_ip_fragment_offset),
    .tx_icmp_ip_ttl(tx_icmp_ip_ttl),
    .tx_icmp_ip_header_checksum(tx_icmp_ip_header_checksum),
    .tx_icmp_ip_source_ip(tx_icmp_ip_source_ip),
    .tx_icmp_ip_dest_ip(tx_icmp_ip_dest_ip),
    .tx_icmp_type(tx_icmp_type),
    .tx_icmp_code(tx_icmp_code),
    .tx_icmp_checksum(tx_icmp_checksum),
    .tx_icmp_header(tx_icmp_header),
    .tx_icmp_length(tx_icmp_length),
    .tx_icmp_payload_axis_tdata(tx_icmp_payload_axis_tdata),
    .tx_icmp_payload_axis_tkeep(tx_icmp_payload_axis_tkeep),
    .tx_icmp_payload_axis_tvalid(tx_icmp_payload_axis_tvalid),
    .tx_icmp_payload_axis_tready(tx_icmp_payload_axis_tready),
    .tx_icmp_payload_axis_tlast(tx_icmp_payload_axis_tlast),
    .tx_icmp_payload_axis_tuser(tx_icmp_payload_axis_tuser),
        
    /*
    * ICMP frame output
    */
    .rx_icmp_hdr_valid(rx_icmp_hdr_valid),
    .rx_icmp_hdr_ready(rx_icmp_hdr_ready),
    .rx_icmp_eth_dest_mac(rx_icmp_eth_dest_mac),
    .rx_icmp_eth_src_mac(rx_icmp_eth_src_mac),
    .rx_icmp_eth_type(rx_icmp_eth_type),
    .rx_icmp_ip_version(rx_icmp_ip_version),
    .rx_icmp_ip_ihl(rx_icmp_ip_ihl),
    .rx_icmp_ip_dscp(rx_icmp_ip_dscp),
    .rx_icmp_ip_ecn(rx_icmp_ip_ecn),
    .rx_icmp_ip_length(rx_icmp_ip_length),
    .rx_icmp_ip_identification(rx_icmp_ip_identification),
    .rx_icmp_ip_flags(rx_icmp_ip_flags),
    .rx_icmp_ip_fragment_offset(rx_icmp_ip_fragment_offset),
    .rx_icmp_ip_ttl(rx_icmp_ip_ttl),
    .rx_icmp_ip_protocol(rx_icmp_ip_protocol),
    .rx_icmp_ip_header_checksum(rx_icmp_ip_header_checksum),
    .rx_icmp_ip_source_ip(rx_icmp_ip_source_ip),
    .rx_icmp_ip_dest_ip(rx_icmp_ip_dest_ip),
    .rx_icmp_type(rx_icmp_type),
    .rx_icmp_code(rx_icmp_code),
    .rx_icmp_checksum(rx_icmp_checksum),
    .rx_icmp_header(rx_icmp_header),
    .rx_icmp_length(rx_icmp_length),
    .rx_icmp_payload_axis_tdata(rx_icmp_payload_axis_tdata),
    .rx_icmp_payload_axis_tkeep(rx_icmp_payload_axis_tkeep),
    .rx_icmp_payload_axis_tvalid(rx_icmp_payload_axis_tvalid),
    .rx_icmp_payload_axis_tready(rx_icmp_payload_axis_tready),
    .rx_icmp_payload_axis_tlast(rx_icmp_payload_axis_tlast),
    .rx_icmp_payload_axis_tuser(rx_icmp_payload_axis_tuser)
);

endmodule

`resetall
