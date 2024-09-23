-- 
-- Copyright (c) 2019 Alex Forencich
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
-- AXI4-Stream asynchronous FIFO with width converter
-- 
entity axis_async_fifo_adapter is
    generic (
        -- FIFO depth in words
        -- KEEP_WIDTH words per cycle if KEEP_ENABLE set
        -- Rounded up to nearest power of 2 cycles
        DEPTH                : integer          := 4096;
        -- Width of input AXI stream interface in bits
        S_DATA_WIDTH         : integer          := 8;
        -- Propagate tkeep signal on input interface
        -- If disabled, tkeep assumed to be 1'b1
        S_KEEP_ENABLE        : integer          := ternary(S_DATA_WIDTH > 8, 1, 0);
        -- tkeep signal width (words per cycle) on input interface
        S_KEEP_WIDTH         : integer          := ((S_DATA_WIDTH + 7)/8);
        -- Width of output AXI stream interface in bits
        M_DATA_WIDTH         : integer          := 8;
        -- Propagate tkeep signal on output interface
        -- If disabled, tkeep assumed to be 1'b1
        M_KEEP_ENABLE        : integer          := ternary(M_DATA_WIDTH > 8, 1, 0);
        -- tkeep signal width (words per cycle) on output interface
        M_KEEP_WIDTH         : integer          := ((M_DATA_WIDTH + 7)/8);
        -- Propagate tid signal
        ID_ENABLE            : integer          := 0;
        -- tid signal width
        ID_WIDTH             : integer          := 8;
        -- Propagate tdest signal
        DEST_ENABLE          : integer          := 0;
        -- tdest signal width
        DEST_WIDTH           : integer          := 8;
        -- Propagate tuser signal
        USER_ENABLE          : integer          := 1;
        -- tuser signal width
        USER_WIDTH           : integer          := 1;
        -- number of RAM pipeline registers in FIFO
        RAM_PIPELINE         : integer          := 1;
        -- use output FIFO
        -- When set, the RAM read enable and pipeline clock enables are removed
        OUTPUT_FIFO_ENABLE   : integer          := 0;
        -- Frame FIFO mode - operate on frames instead of cycles
        -- When set, m_axis_tvalid will not be deasserted within a frame
        -- Requires LAST_ENABLE set
        FRAME_FIFO           : integer          := 0;
        -- tuser value for bad frame marker
        USER_BAD_FRAME_VALUE : std_logic_vector := "1";
        -- tuser mask for bad frame marker
        USER_BAD_FRAME_MASK  : std_logic_vector := "1";
        -- Drop frames larger than FIFO
        -- Requires FRAME_FIFO set
        DROP_OVERSIZE_FRAME  : integer          := FRAME_FIFO;
        -- Drop frames marked bad
        -- Requires FRAME_FIFO and DROP_OVERSIZE_FRAME set
        DROP_BAD_FRAME       : integer          := 0;
        -- Drop incoming frames when full
        -- When set, s_axis_tready is always asserted
        -- Requires FRAME_FIFO and DROP_OVERSIZE_FRAME set
        DROP_WHEN_FULL       : integer          := 0;
        -- Mark incoming frames as bad frames when full
        -- When set, s_axis_tready is always asserted
        -- Requires FRAME_FIFO to be clear
        MARK_WHEN_FULL       : integer          := 0;
        -- Enable pause request input
        PAUSE_ENABLE         : integer          := 0;
        -- Pause between frames
        FRAME_PAUSE          : integer          := FRAME_FIFO
    );
    port (
        --
        -- AXI input
        --
        signal s_clk                 : in  std_logic;
        signal s_rst                 : in  std_logic;
        signal s_axis_tdata          : in  std_logic_vector(S_DATA_WIDTH - 1 downto 0);
        signal s_axis_tkeep          : in  std_logic_vector(S_KEEP_WIDTH - 1 downto 0);
        signal s_axis_tvalid         : in  std_logic;
        signal s_axis_tready         : out std_logic;
        signal s_axis_tlast          : in  std_logic;
        signal s_axis_tid            : in  std_logic_vector(ID_WIDTH - 1 downto 0);
        signal s_axis_tdest          : in  std_logic_vector(DEST_WIDTH - 1 downto 0);
        signal s_axis_tuser          : in  std_logic_vector(USER_WIDTH - 1 downto 0);

        --
        -- AXI output
        --
        signal m_clk                 : in  std_logic;
        signal m_rst                 : in  std_logic;
        signal m_axis_tdata          : out std_logic_vector(M_DATA_WIDTH - 1 downto 0);
        signal m_axis_tkeep          : out std_logic_vector(M_KEEP_WIDTH - 1 downto 0);
        signal m_axis_tvalid         : out std_logic;
        signal m_axis_tready         : in  std_logic;
        signal m_axis_tlast          : out std_logic;
        signal m_axis_tid            : out std_logic_vector(ID_WIDTH - 1 downto 0);
        signal m_axis_tdest          : out std_logic_vector(DEST_WIDTH - 1 downto 0);
        signal m_axis_tuser          : out std_logic_vector(USER_WIDTH - 1 downto 0);

        --
        -- Pause
        --
        signal s_pause_req           : in  std_logic := '0';
        signal s_pause_ack           : out std_logic;
        signal m_pause_req           : in  std_logic := '0';
        signal m_pause_ack           : out std_logic;

        --
        -- Status
        --
        signal s_status_depth        : out std_logic_vector(clog2(DEPTH) downto 0);
        signal s_status_depth_commit : out std_logic_vector(clog2(DEPTH) downto 0);
        signal s_status_overflow     : out std_logic;
        signal s_status_bad_frame    : out std_logic;
        signal s_status_good_frame   : out std_logic;
        signal m_status_depth        : out std_logic_vector(clog2(DEPTH) downto 0);
        signal m_status_depth_commit : out std_logic_vector(clog2(DEPTH) downto 0);
        signal m_status_overflow     : out std_logic;
        signal m_status_bad_frame    : out std_logic;
        signal m_status_good_frame   : out std_logic
    );
end entity;

architecture rtl of axis_async_fifo_adapter is

    -- force keep width to 1 when disabled
    constant S_BYTE_LANES        : integer := ternary(S_KEEP_ENABLE, S_KEEP_WIDTH, 1);
    constant M_BYTE_LANES        : integer := ternary(M_KEEP_ENABLE, M_KEEP_WIDTH, 1);

    -- bus byte sizes (must be identical)
    constant S_BYTE_SIZE         : integer := S_DATA_WIDTH / S_BYTE_LANES;
    constant M_BYTE_SIZE         : integer := M_DATA_WIDTH / M_BYTE_LANES;

    -- output bus is wider
    constant EXPAND_BUS          : integer := ternary(M_BYTE_LANES > S_BYTE_LANES, 1, 0);

    -- total data and keep widths
    constant DATA_WIDTH          : integer := ternary(EXPAND_BUS, M_DATA_WIDTH, S_DATA_WIDTH);
    constant KEEP_WIDTH          : integer := ternary(EXPAND_BUS, M_BYTE_LANES, S_BYTE_LANES);

    signal pre_fifo_axis_tdata   : std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal pre_fifo_axis_tkeep   : std_logic_vector(KEEP_WIDTH - 1 downto 0);
    signal pre_fifo_axis_tvalid  : std_logic;
    signal pre_fifo_axis_tready  : std_logic;
    signal pre_fifo_axis_tlast   : std_logic;
    signal pre_fifo_axis_tid     : std_logic_vector(ID_WIDTH - 1 downto 0);
    signal pre_fifo_axis_tdest   : std_logic_vector(DEST_WIDTH - 1 downto 0);
    signal pre_fifo_axis_tuser   : std_logic_vector(USER_WIDTH - 1 downto 0);

    signal post_fifo_axis_tdata  : std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal post_fifo_axis_tkeep  : std_logic_vector(KEEP_WIDTH - 1 downto 0);
    signal post_fifo_axis_tvalid : std_logic;
    signal post_fifo_axis_tready : std_logic;
    signal post_fifo_axis_tlast  : std_logic;
    signal post_fifo_axis_tid    : std_logic_vector(ID_WIDTH - 1 downto 0);
    signal post_fifo_axis_tdest  : std_logic_vector(DEST_WIDTH - 1 downto 0);
    signal post_fifo_axis_tuser  : std_logic_vector(USER_WIDTH - 1 downto 0);
begin

    -- bus width assertions
    P_check : process (all) begin
        if S_BYTE_SIZE * S_BYTE_LANES /= S_DATA_WIDTH then
            report "Error: in data width not evenly divisible"
                severity failure;
        end if;

        if M_BYTE_SIZE * M_BYTE_LANES /= M_DATA_WIDTH then
            report "Error: output data width not evenly divisible"
                severity failure;
        end if;

        if S_BYTE_SIZE /= M_BYTE_SIZE then
            report "Error: byte size mismatch"
                severity failure;
        end if;
    end process;

    g_upsize_pre : if (M_BYTE_LANES > S_BYTE_LANES) generate

        -- output wider, adapt width before FIFO

        adapter_inst : entity work.axis_adapter
            generic map(
                S_DATA_WIDTH  => S_DATA_WIDTH,
                S_KEEP_ENABLE => S_KEEP_ENABLE,
                S_KEEP_WIDTH  => S_KEEP_WIDTH,
                M_DATA_WIDTH  => M_DATA_WIDTH,
                M_KEEP_ENABLE => M_KEEP_ENABLE,
                M_KEEP_WIDTH  => M_KEEP_WIDTH,
                ID_ENABLE     => ID_ENABLE,
                ID_WIDTH      => ID_WIDTH,
                DEST_ENABLE   => DEST_ENABLE,
                DEST_WIDTH    => DEST_WIDTH,
                USER_ENABLE   => USER_ENABLE,
                USER_WIDTH    => USER_WIDTH
            )
            port map(
                clk           => s_clk,
                rst           => s_rst,
                -- AXI input
                s_axis_tdata  => s_axis_tdata,
                s_axis_tkeep  => s_axis_tkeep,
                s_axis_tvalid => s_axis_tvalid,
                s_axis_tready => s_axis_tready,
                s_axis_tlast  => s_axis_tlast,
                s_axis_tid    => s_axis_tid,
                s_axis_tdest  => s_axis_tdest,
                s_axis_tuser  => s_axis_tuser,
                -- AXI output
                m_axis_tdata  => pre_fifo_axis_tdata,
                m_axis_tkeep  => pre_fifo_axis_tkeep,
                m_axis_tvalid => pre_fifo_axis_tvalid,
                m_axis_tready => pre_fifo_axis_tready,
                m_axis_tlast  => pre_fifo_axis_tlast,
                m_axis_tid    => pre_fifo_axis_tid,
                m_axis_tdest  => pre_fifo_axis_tdest,
                m_axis_tuser  => pre_fifo_axis_tuser
            );
    end generate;

    g_bypass_pre : if not(M_BYTE_LANES > S_BYTE_LANES) generate

        pre_fifo_axis_tdata  <= s_axis_tdata;
        pre_fifo_axis_tkeep  <= s_axis_tkeep;
        pre_fifo_axis_tvalid <= s_axis_tvalid;
        s_axis_tready        <= pre_fifo_axis_tready;
        pre_fifo_axis_tlast  <= s_axis_tlast;
        pre_fifo_axis_tid    <= s_axis_tid;
        pre_fifo_axis_tdest  <= s_axis_tdest;
        pre_fifo_axis_tuser  <= s_axis_tuser;

    end generate;

    fifo_inst : entity work.axis_async_fifo
        generic map(
            DEPTH                => DEPTH,
            DATA_WIDTH           => DATA_WIDTH,
            KEEP_ENABLE          => ternary(EXPAND_BUS, M_KEEP_ENABLE, S_KEEP_ENABLE),
            KEEP_WIDTH           => KEEP_WIDTH,
            LAST_ENABLE          => 1,
            ID_ENABLE            => ID_ENABLE,
            ID_WIDTH             => ID_WIDTH,
            DEST_ENABLE          => DEST_ENABLE,
            DEST_WIDTH           => DEST_WIDTH,
            USER_ENABLE          => USER_ENABLE,
            USER_WIDTH           => USER_WIDTH,
            RAM_PIPELINE         => RAM_PIPELINE,
            OUTPUT_FIFO_ENABLE   => OUTPUT_FIFO_ENABLE,
            FRAME_FIFO           => FRAME_FIFO,
            USER_BAD_FRAME_VALUE => USER_BAD_FRAME_VALUE,
            USER_BAD_FRAME_MASK  => USER_BAD_FRAME_MASK,
            DROP_OVERSIZE_FRAME  => DROP_OVERSIZE_FRAME,
            DROP_BAD_FRAME       => DROP_BAD_FRAME,
            DROP_WHEN_FULL       => DROP_WHEN_FULL,
            MARK_WHEN_FULL       => MARK_WHEN_FULL,
            PAUSE_ENABLE         => PAUSE_ENABLE,
            FRAME_PAUSE          => FRAME_PAUSE
        )
        port map(
            -- AXI input
            s_clk                 => s_clk,
            s_rst                 => s_rst,
            s_axis_tdata          => pre_fifo_axis_tdata,
            s_axis_tkeep          => pre_fifo_axis_tkeep,
            s_axis_tvalid         => pre_fifo_axis_tvalid,
            s_axis_tready         => pre_fifo_axis_tready,
            s_axis_tlast          => pre_fifo_axis_tlast,
            s_axis_tid            => pre_fifo_axis_tid,
            s_axis_tdest          => pre_fifo_axis_tdest,
            s_axis_tuser          => pre_fifo_axis_tuser,
            -- AXI output
            m_clk                 => m_clk,
            m_rst                 => m_rst,
            m_axis_tdata          => post_fifo_axis_tdata,
            m_axis_tkeep          => post_fifo_axis_tkeep,
            m_axis_tvalid         => post_fifo_axis_tvalid,
            m_axis_tready         => post_fifo_axis_tready,
            m_axis_tlast          => post_fifo_axis_tlast,
            m_axis_tid            => post_fifo_axis_tid,
            m_axis_tdest          => post_fifo_axis_tdest,
            m_axis_tuser          => post_fifo_axis_tuser,
            -- Pause
            s_pause_req           => s_pause_req,
            s_pause_ack           => s_pause_ack,
            m_pause_req           => m_pause_req,
            m_pause_ack           => m_pause_ack,
            -- Status
            s_status_depth        => s_status_depth,
            s_status_depth_commit => s_status_depth_commit,
            s_status_overflow     => s_status_overflow,
            s_status_bad_frame    => s_status_bad_frame,
            s_status_good_frame   => s_status_good_frame,
            m_status_depth        => m_status_depth,
            m_status_depth_commit => m_status_depth_commit,
            m_status_overflow     => m_status_overflow,
            m_status_bad_frame    => m_status_bad_frame,
            m_status_good_frame   => m_status_good_frame
        );

    g_downsize_post : if (M_BYTE_LANES < S_BYTE_LANES) generate

        -- input wider, adapt width after FIFO

        adapter_inst : entity work.axis_adapter
            generic map(
                S_DATA_WIDTH  => S_DATA_WIDTH,
                S_KEEP_ENABLE => S_KEEP_ENABLE,
                S_KEEP_WIDTH  => S_KEEP_WIDTH,
                M_DATA_WIDTH  => M_DATA_WIDTH,
                M_KEEP_ENABLE => M_KEEP_ENABLE,
                M_KEEP_WIDTH  => M_KEEP_WIDTH,
                ID_ENABLE     => ID_ENABLE,
                ID_WIDTH      => ID_WIDTH,
                DEST_ENABLE   => DEST_ENABLE,
                DEST_WIDTH    => DEST_WIDTH,
                USER_ENABLE   => USER_ENABLE,
                USER_WIDTH    => USER_WIDTH
            )
            port map(
                clk           => m_clk,
                rst           => m_rst,
                -- AXI input
                s_axis_tdata  => post_fifo_axis_tdata,
                s_axis_tkeep  => post_fifo_axis_tkeep,
                s_axis_tvalid => post_fifo_axis_tvalid,
                s_axis_tready => post_fifo_axis_tready,
                s_axis_tlast  => post_fifo_axis_tlast,
                s_axis_tid    => post_fifo_axis_tid,
                s_axis_tdest  => post_fifo_axis_tdest,
                s_axis_tuser  => post_fifo_axis_tuser,
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
    end generate;

    g_bypass_post : if not(M_BYTE_LANES < S_BYTE_LANES) generate

        m_axis_tdata          <= post_fifo_axis_tdata;
        m_axis_tkeep          <= post_fifo_axis_tkeep;
        m_axis_tvalid         <= post_fifo_axis_tvalid;
        post_fifo_axis_tready <= m_axis_tready;
        m_axis_tlast          <= post_fifo_axis_tlast;
        m_axis_tid            <= post_fifo_axis_tid;
        m_axis_tdest          <= post_fifo_axis_tdest;
        m_axis_tuser          <= post_fifo_axis_tuser;

    end generate;

end architecture;