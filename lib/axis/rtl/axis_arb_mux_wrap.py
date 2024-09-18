#!/usr/bin/env python3
"""
Generates an AXI Stream arbitrated mux wrapper with the specified number of ports
"""

import argparse
from jinja2 import Template


def main():
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('-p', '--ports',  type=int, default=4, help="number of ports")
    parser.add_argument('-n', '--name',   type=str, help="module name")
    parser.add_argument('-o', '--output', type=str, help="output file name")
    parser.add_argument('-X', '--vhdl', default=False, action='store_true', help="Generate VHDL wrapper")

    args = parser.parse_args()

    try:
        if args.vhdl:
            del args.vhdl
            generate_vhdl(**args.__dict__)
        else:
            del args.vhdl
            generate_verilog(**args.__dict__)
    except IOError as ex:
        print(ex)
        exit(1)


def generate_verilog(ports=4, name=None, output=None):
    n = ports

    if name is None:
        name = "axis_arb_mux_wrap_{0}".format(n)

    if output is None:
        output = name + ".v"

    print("Generating {0} port AXI stream arbitrated mux wrapper {1}...".format(n, name))

    cn = (n-1).bit_length()

    t = Template(u"""/*

Copyright (c) 2018-2021 Alex Forencich

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
 * AXI4-Stream {{n}} port arbitrated mux (wrapper)
 */
module {{name}} #
(
    // Width of AXI stream interfaces in bits
    parameter DATA_WIDTH = 8,
    // Propagate tkeep signal
    parameter KEEP_ENABLE = (DATA_WIDTH>8),
    // tkeep signal width (words per cycle)
    parameter KEEP_WIDTH = ((DATA_WIDTH+7)/8),
    // Propagate tid signal
    parameter ID_ENABLE = 0,
    // input tid signal width
    parameter S_ID_WIDTH = 8,
    // output tid signal width
    parameter M_ID_WIDTH = S_ID_WIDTH+{{cn}},
    // Propagate tdest signal
    parameter DEST_ENABLE = 0,
    // tdest signal width
    parameter DEST_WIDTH = 8,
    // Propagate tuser signal
    parameter USER_ENABLE = 1,
    // tuser signal width
    parameter USER_WIDTH = 1,
    // Propagate tlast signal
    parameter LAST_ENABLE = 1,
    // Update tid with routing information
    parameter UPDATE_TID = 0,
    // select round robin arbitration
    parameter ARB_TYPE_ROUND_ROBIN = 0,
    // LSB priority selection
    parameter ARB_LSB_HIGH_PRIORITY = 1
)
(
    input  wire                   clk,
    input  wire                   rst,

    /*
     * AXI Stream inputs
     */
{%- for p in range(n) %}
    input  wire [DATA_WIDTH-1:0]  s{{'%02d'%p}}_axis_tdata,
    input  wire [KEEP_WIDTH-1:0]  s{{'%02d'%p}}_axis_tkeep,
    input  wire                   s{{'%02d'%p}}_axis_tvalid,
    output wire                   s{{'%02d'%p}}_axis_tready,
    input  wire                   s{{'%02d'%p}}_axis_tlast,
    input  wire [S_ID_WIDTH-1:0]  s{{'%02d'%p}}_axis_tid,
    input  wire [DEST_WIDTH-1:0]  s{{'%02d'%p}}_axis_tdest,
    input  wire [USER_WIDTH-1:0]  s{{'%02d'%p}}_axis_tuser,
{% endfor %}
    /*
     * AXI Stream output
     */
    output wire [DATA_WIDTH-1:0]  m_axis_tdata,
    output wire [KEEP_WIDTH-1:0]  m_axis_tkeep,
    output wire                   m_axis_tvalid,
    input  wire                   m_axis_tready,
    output wire                   m_axis_tlast,
    output wire [M_ID_WIDTH-1:0]  m_axis_tid,
    output wire [DEST_WIDTH-1:0]  m_axis_tdest,
    output wire [USER_WIDTH-1:0]  m_axis_tuser
);

axis_arb_mux #(
    .S_COUNT({{n}}),
    .DATA_WIDTH(DATA_WIDTH),
    .KEEP_ENABLE(KEEP_ENABLE),
    .KEEP_WIDTH(KEEP_WIDTH),
    .ID_ENABLE(ID_ENABLE),
    .S_ID_WIDTH(S_ID_WIDTH),
    .M_ID_WIDTH(M_ID_WIDTH),
    .DEST_ENABLE(DEST_ENABLE),
    .DEST_WIDTH(DEST_WIDTH),
    .USER_ENABLE(USER_ENABLE),
    .USER_WIDTH(USER_WIDTH),
    .LAST_ENABLE(LAST_ENABLE),
    .UPDATE_TID(UPDATE_TID),
    .ARB_TYPE_ROUND_ROBIN(ARB_TYPE_ROUND_ROBIN),
    .ARB_LSB_HIGH_PRIORITY(ARB_LSB_HIGH_PRIORITY)
)
axis_arb_mux_inst (
    .clk(clk),
    .rst(rst),
    // AXI inputs
    .s_axis_tdata({ {% for p in range(n-1,-1,-1) %}s{{'%02d'%p}}_axis_tdata{% if not loop.last %}, {% endif %}{% endfor %} }),
    .s_axis_tkeep({ {% for p in range(n-1,-1,-1) %}s{{'%02d'%p}}_axis_tkeep{% if not loop.last %}, {% endif %}{% endfor %} }),
    .s_axis_tvalid({ {% for p in range(n-1,-1,-1) %}s{{'%02d'%p}}_axis_tvalid{% if not loop.last %}, {% endif %}{% endfor %} }),
    .s_axis_tready({ {% for p in range(n-1,-1,-1) %}s{{'%02d'%p}}_axis_tready{% if not loop.last %}, {% endif %}{% endfor %} }),
    .s_axis_tlast({ {% for p in range(n-1,-1,-1) %}s{{'%02d'%p}}_axis_tlast{% if not loop.last %}, {% endif %}{% endfor %} }),
    .s_axis_tid({ {% for p in range(n-1,-1,-1) %}s{{'%02d'%p}}_axis_tid{% if not loop.last %}, {% endif %}{% endfor %} }),
    .s_axis_tdest({ {% for p in range(n-1,-1,-1) %}s{{'%02d'%p}}_axis_tdest{% if not loop.last %}, {% endif %}{% endfor %} }),
    .s_axis_tuser({ {% for p in range(n-1,-1,-1) %}s{{'%02d'%p}}_axis_tuser{% if not loop.last %}, {% endif %}{% endfor %} }),
    // AXI output
    .m_axis_tdata(m_axis_tdata),
    .m_axis_tkeep(m_axis_tkeep),
    .m_axis_tvalid(m_axis_tvalid),
    .m_axis_tready(m_axis_tready),
    .m_axis_tlast(m_axis_tlast),
    .m_axis_tid(m_axis_tid),
    .m_axis_tdest(m_axis_tdest),
    .m_axis_tuser(m_axis_tuser)
);

endmodule

`resetall

""")

    print(f"Writing file '{output}'...")

    with open(output, 'w') as f:
        f.write(t.render(
            n=n,
            cn=cn,
            name=name
        ))
        f.flush()

    print("Done")

def generate_vhdl(ports=4, name=None, output=None):
    n = ports

    if name is None:
        name = "axis_arb_mux_wrap_{0}".format(n)

    if output is None:
        output = name + ".vhd"

    print("Generating {0} port AXI stream arbitrated mux wrapper {1}...".format(n, name))

    cn = (n-1).bit_length()

    t = Template(u"""-- 
-- 
-- Copyright (c) 2018-2021 Alex Forencich
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

use work.vhdl_pkg.all;

--
-- AXI4-Stream {{n}} port arbitrated mux (wrapper)
--
entity {{name}} is
    generic (
        -- Width of AXI stream interfaces in bits
        DATA_WIDTH : integer := 8;
        -- Propagate tkeep signal
        KEEP_ENABLE : integer := ternary(DATA_WIDTH>8, 1, 0);
        -- tkeep signal width (words per cycle)
        KEEP_WIDTH : integer := ((DATA_WIDTH+7)/8);
        -- Propagate tid signal
        ID_ENABLE : integer := 0;
        -- input tid signal width
        S_ID_WIDTH : integer := 8;
        -- output tid signal width
        M_ID_WIDTH : integer := S_ID_WIDTH+{{cn}};
        -- Propagate tdest signal
        DEST_ENABLE : integer := 0;
        -- tdest signal width
        DEST_WIDTH : integer := 8;
        -- Propagate tuser signal
        USER_ENABLE : integer := 1;
        -- tuser signal width
        USER_WIDTH : integer := 1;
        -- Propagate tlast signal
        LAST_ENABLE : integer := 1;
        -- Update tid with routing information
        UPDATE_TID : integer := 0;
        -- select round robin arbitration
        ARB_TYPE_ROUND_ROBIN : integer := 0;
        -- LSB priority selection
        ARB_LSB_HIGH_PRIORITY : integer := 1
    );
    port(
        signal clk                          : in  std_logic;
        signal rst                          : in  std_logic;

        -- 
        -- AXI Stream inputs
        -- 
    {%- for p in range(n) %}
        signal s{{'%02d'%p}}_axis_tdata     : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        signal s{{'%02d'%p}}_axis_tkeep     : in  std_logic_vector(KEEP_WIDTH-1 downto 0);
        signal s{{'%02d'%p}}_axis_tvalid    : in  std_logic;
        signal s{{'%02d'%p}}_axis_tready    : out std_logic;
        signal s{{'%02d'%p}}_axis_tlast     : in  std_logic;
        signal s{{'%02d'%p}}_axis_tid       : in  std_logic_vector(S_ID_WIDTH-1 downto 0);
        signal s{{'%02d'%p}}_axis_tdest     : in  std_logic_vector(DEST_WIDTH-1 downto 0);
        signal s{{'%02d'%p}}_axis_tuser     : in  std_logic_vector(USER_WIDTH-1 downto 0);
    {% endfor %}

        -- 
        -- AXI Stream output
        -- 
        signal m_axis_tdata                 : out std_logic_vector(DATA_WIDTH-1 downto 0);
        signal m_axis_tkeep                 : out std_logic_vector(KEEP_WIDTH-1 downto 0);
        signal m_axis_tvalid                : out std_logic;
        signal m_axis_tready                : in  std_logic;
        signal m_axis_tlast                 : out std_logic;
        signal m_axis_tid                   : out std_logic_vector(M_ID_WIDTH-1 downto 0);
        signal m_axis_tdest                 : out std_logic_vector(DEST_WIDTH-1 downto 0);
        signal m_axis_tuser                 : out std_logic_vector(USER_WIDTH-1 downto 0)
    );
end entity;

architecture gen of {{name}} is
        signal s_axis_tdata  : std_logic_vector({{n}}*DATA_WIDTH-1 downto 0);
        signal s_axis_tkeep  : std_logic_vector({{n}}*KEEP_WIDTH-1 downto 0);
        signal s_axis_tvalid : std_logic_vector({{n}}-1 downto 0);
        signal s_axis_tready : std_logic_vector({{n}}-1 downto 0);
        signal s_axis_tlast  : std_logic_vector({{n}}-1 downto 0);
        signal s_axis_tid    : std_logic_vector({{n}}*S_ID_WIDTH-1 downto 0);
        signal s_axis_tdest  : std_logic_vector({{n}}*DEST_WIDTH-1 downto 0);
        signal s_axis_tuser  : std_logic_vector({{n}}*USER_WIDTH-1 downto 0);
begin

    axis_arb_mux_inst : entity work.axis_arb_mux
        generic map(
            S_COUNT               => {{n}},
            DATA_WIDTH            => DATA_WIDTH,
            KEEP_ENABLE           => KEEP_ENABLE,
            KEEP_WIDTH            => KEEP_WIDTH,
            ID_ENABLE             => ID_ENABLE,
            S_ID_WIDTH            => S_ID_WIDTH,
            M_ID_WIDTH            => M_ID_WIDTH,
            DEST_ENABLE           => DEST_ENABLE,
            DEST_WIDTH            => DEST_WIDTH,
            USER_ENABLE           => USER_ENABLE,
            USER_WIDTH            => USER_WIDTH,
            LAST_ENABLE           => LAST_ENABLE,
            UPDATE_TID            => UPDATE_TID,
            ARB_TYPE_ROUND_ROBIN  => ARB_TYPE_ROUND_ROBIN,
            ARB_LSB_HIGH_PRIORITY => ARB_LSB_HIGH_PRIORITY
        )
        port map(
            clk => clk,
            rst => rst,

            -- AXI inputs
            s_axis_tdata  => s_axis_tdata,
            s_axis_tkeep  => s_axis_tkeep,
            s_axis_tvalid => s_axis_tvalid,
            s_axis_tready => s_axis_tready,
            s_axis_tlast  => s_axis_tlast,
            s_axis_tid    => s_axis_tid,
            s_axis_tdest  => s_axis_tdest,
            s_axis_tuser  => s_axis_tuser,

            -- AXI output
            m_axis_tdata  => m_axis_tdata,
            m_axis_tkeep  => m_axis_tkeep,
            m_axis_tvalid => m_axis_tvalid,
            m_axis_tready => m_axis_tready,
            m_axis_tlast  => m_axis_tlast,
            m_axis_tid    => m_axis_tid,
            m_axis_tdest  => m_axis_tdest,
            m_axis_tuser  => m_axis_tuser
        );

    {%- for p in range(n) %}
        s_axis_tdata(({{p}}+1)*DATA_WIDTH-1 downto {{p}}*DATA_WIDTH) <= s{{'%02d'%p}}_axis_tdata;
        s_axis_tkeep(({{p}}+1)*KEEP_WIDTH-1 downto {{p}}*KEEP_WIDTH) <= s{{'%02d'%p}}_axis_tkeep;
        s_axis_tvalid({{p}}) <= s{{'%02d'%p}}_axis_tvalid;
        s{{'%02d'%p}}_axis_tready <= s_axis_tready({{p}});
        s_axis_tlast({{p}}) <= s{{'%02d'%p}}_axis_tlast;
        s_axis_tid(({{p}}+1)*S_ID_WIDTH-1 downto {{p}}*S_ID_WIDTH)  <= s{{'%02d'%p}}_axis_tid;
        s_axis_tdest(({{p}}+1)*DEST_WIDTH-1 downto {{p}}*DEST_WIDTH) <= s{{'%02d'%p}}_axis_tdest;
        s_axis_tuser(({{p}}+1)*USER_WIDTH-1 downto {{p}}*USER_WIDTH) <= s{{'%02d'%p}}_axis_tuser;
    {% endfor %}
end architecture;
""")

    print(f"Writing file '{output}'...")

    with open(output, 'w') as f:
        f.write(t.render(
            n=n,
            cn=cn,
            name=name
        ))
        f.flush()

    print("Done")


if __name__ == "__main__":
    main()
