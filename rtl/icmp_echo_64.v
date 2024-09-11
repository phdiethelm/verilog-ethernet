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
 * ICMP echo block, ICMP interface (64 bit datapath)
 */
module icmp_echo_64
(
    input  wire        clk,
    input  wire        rst,

    /*
    * ICMP frame input
    */
    output  wire        tx_icmp_hdr_valid,
    input   wire        tx_icmp_hdr_ready,
    output  wire [47:0] tx_icmp_eth_dest_mac,
    output  wire [47:0] tx_icmp_eth_src_mac,
    output  wire [15:0] tx_icmp_eth_type,
    output  wire [3:0]  tx_icmp_ip_version,
    output  wire [3:0]  tx_icmp_ip_ihl,
    output  wire [5:0]  tx_icmp_ip_dscp,
    output  wire [1:0]  tx_icmp_ip_ecn,
    output  wire [15:0] tx_icmp_ip_identification,
    output  wire [2:0]  tx_icmp_ip_flags,
    output  wire [12:0] tx_icmp_ip_fragment_offset,
    output  wire [7:0]  tx_icmp_ip_ttl,
    output  wire [15:0] tx_icmp_ip_header_checksum,
    output  wire [31:0] tx_icmp_ip_source_ip,
    output  wire [31:0] tx_icmp_ip_dest_ip,
    output  wire [7:0]  tx_icmp_type,
    output  wire [7:0]  tx_icmp_code,
    output  wire [15:0] tx_icmp_checksum,
    output  wire [31:0] tx_icmp_header,
    output  wire [15:0] tx_icmp_length,
    output  wire [63:0] tx_icmp_payload_axis_tdata,
    output  wire [7:0]  tx_icmp_payload_axis_tkeep,
    output  wire        tx_icmp_payload_axis_tvalid,
    input   wire        tx_icmp_payload_axis_tready,
    output  wire        tx_icmp_payload_axis_tlast,
    output  wire        tx_icmp_payload_axis_tuser,

    /*
    * ICMP frame output
    */
    input   wire        rx_icmp_hdr_valid,
    output  wire        rx_icmp_hdr_ready,
    input   wire [47:0] rx_icmp_eth_dest_mac,
    input   wire [47:0] rx_icmp_eth_src_mac,
    input   wire [15:0] rx_icmp_eth_type,
    input   wire [3:0]  rx_icmp_ip_version,
    input   wire [3:0]  rx_icmp_ip_ihl,
    input   wire [5:0]  rx_icmp_ip_dscp,
    input   wire [1:0]  rx_icmp_ip_ecn,
    input   wire [15:0] rx_icmp_ip_length,
    input   wire [15:0] rx_icmp_ip_identification,
    input   wire [2:0]  rx_icmp_ip_flags,
    input   wire [12:0] rx_icmp_ip_fragment_offset,
    input   wire [7:0]  rx_icmp_ip_ttl,
    input   wire [7:0]  rx_icmp_ip_protocol,
    input   wire [15:0] rx_icmp_ip_header_checksum,
    input   wire [31:0] rx_icmp_ip_source_ip,
    input   wire [31:0] rx_icmp_ip_dest_ip,
    input   wire [7:0]  rx_icmp_type,
    input   wire [7:0]  rx_icmp_code,
    input   wire [15:0] rx_icmp_checksum,
    input   wire [31:0] rx_icmp_header,
    input   wire [15:0] rx_icmp_length,
    input   wire [63:0] rx_icmp_payload_axis_tdata,
    input   wire [7:0]  rx_icmp_payload_axis_tkeep,
    input   wire        rx_icmp_payload_axis_tvalid,
    output  wire        rx_icmp_payload_axis_tready,
    input   wire        rx_icmp_payload_axis_tlast,
    input   wire        rx_icmp_payload_axis_tuser
);

wire [63:0] rx_fifo_icmp_payload_axis_tdata;
wire [7:0]  rx_fifo_icmp_payload_axis_tkeep;
wire        rx_fifo_icmp_payload_axis_tvalid;
wire        rx_fifo_icmp_payload_axis_tready;
wire        rx_fifo_icmp_payload_axis_tlast;
wire        rx_fifo_icmp_payload_axis_tuser;

wire [63:0] tx_fifo_icmp_payload_axis_tdata;
wire [7:0]  tx_fifo_icmp_payload_axis_tkeep;
wire        tx_fifo_icmp_payload_axis_tvalid;
wire        tx_fifo_icmp_payload_axis_tready;
wire        tx_fifo_icmp_payload_axis_tlast;
wire        tx_fifo_icmp_payload_axis_tuser;

// Loop back ICMP ECHO
wire match_cond = rx_icmp_type == 8 && rx_icmp_code == 0;
wire no_match = ~match_cond;

reg match_cond_reg = 0;
reg no_match_reg = 0;

always @(posedge clk) begin
    if (rst) begin
        match_cond_reg <= 0;
        no_match_reg <= 0;
    end else begin
        if (rx_icmp_payload_axis_tvalid) begin
            if ((~match_cond_reg & ~no_match_reg) |
                (rx_icmp_payload_axis_tvalid & rx_icmp_payload_axis_tready & rx_icmp_payload_axis_tlast)) begin
                match_cond_reg <= match_cond;
                no_match_reg <= no_match;
            end
        end else begin
            match_cond_reg <= 0;
            no_match_reg <= 0;
        end
    end
end

assign tx_icmp_hdr_valid = rx_icmp_hdr_valid & match_cond;
assign rx_icmp_hdr_ready = (tx_icmp_hdr_ready & match_cond) | no_match;
assign tx_icmp_ip_dscp = 0;
assign tx_icmp_ip_ecn = 0;
assign tx_icmp_ip_ttl = rx_icmp_ip_ttl;
assign tx_icmp_ip_source_ip = rx_icmp_ip_dest_ip;
assign tx_icmp_ip_dest_ip = rx_icmp_ip_source_ip;
assign tx_icmp_type = 8'd0; // ECHO reply
assign tx_icmp_code = 8'd0;
assign tx_icmp_header = rx_icmp_header;
assign tx_icmp_length = rx_icmp_length;
assign tx_icmp_checksum = 0;

assign tx_icmp_payload_axis_tdata = tx_fifo_icmp_payload_axis_tdata;
assign tx_icmp_payload_axis_tkeep = tx_fifo_icmp_payload_axis_tkeep;
assign tx_icmp_payload_axis_tvalid = tx_fifo_icmp_payload_axis_tvalid;
assign tx_fifo_icmp_payload_axis_tready = tx_icmp_payload_axis_tready;
assign tx_icmp_payload_axis_tlast = tx_fifo_icmp_payload_axis_tlast;
assign tx_icmp_payload_axis_tuser = tx_fifo_icmp_payload_axis_tuser;

assign rx_fifo_icmp_payload_axis_tdata = rx_icmp_payload_axis_tdata;
assign rx_fifo_icmp_payload_axis_tkeep = rx_icmp_payload_axis_tkeep;
assign rx_fifo_icmp_payload_axis_tvalid = rx_icmp_payload_axis_tvalid & match_cond_reg;
assign rx_icmp_payload_axis_tready = (rx_fifo_icmp_payload_axis_tready & match_cond_reg) | no_match_reg;
assign rx_fifo_icmp_payload_axis_tlast = rx_icmp_payload_axis_tlast;
assign rx_fifo_icmp_payload_axis_tuser = rx_icmp_payload_axis_tuser;

axis_fifo #(
    .DEPTH(8192),
    .DATA_WIDTH(64),
    .KEEP_ENABLE(1),
    .KEEP_WIDTH(8),
    .ID_ENABLE(0),
    .DEST_ENABLE(0),
    .USER_ENABLE(1),
    .USER_WIDTH(1),
    .FRAME_FIFO(0)
)
icmp_payload_fifo (
    .clk(clk),
    .rst(rst),

    // AXI input
    .s_axis_tdata(rx_fifo_icmp_payload_axis_tdata),
    .s_axis_tkeep(rx_fifo_icmp_payload_axis_tkeep),
    .s_axis_tvalid(rx_fifo_icmp_payload_axis_tvalid),
    .s_axis_tready(rx_fifo_icmp_payload_axis_tready),
    .s_axis_tlast(rx_fifo_icmp_payload_axis_tlast),
    .s_axis_tid(0),
    .s_axis_tdest(0),
    .s_axis_tuser(rx_fifo_icmp_payload_axis_tuser),

    // AXI output
    .m_axis_tdata(tx_fifo_icmp_payload_axis_tdata),
    .m_axis_tkeep(tx_fifo_icmp_payload_axis_tkeep),
    .m_axis_tvalid(tx_fifo_icmp_payload_axis_tvalid),
    .m_axis_tready(tx_fifo_icmp_payload_axis_tready),
    .m_axis_tlast(tx_fifo_icmp_payload_axis_tlast),
    .m_axis_tid(),
    .m_axis_tdest(),
    .m_axis_tuser(tx_fifo_icmp_payload_axis_tuser),

    // Status
    .status_overflow(),
    .status_bad_frame(),
    .status_good_frame()
);

endmodule

`resetall
