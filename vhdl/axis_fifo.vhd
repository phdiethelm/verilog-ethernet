-- 
-- Copyright (c) 2013-2023 Alex Forencich
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
-- AXI4-Stream FIFO
--
entity axis_fifo is
    generic (
        -- FIFO depth in words
        -- KEEP_WIDTH words per cycle if KEEP_ENABLE set
        -- Rounded up to nearest power of 2 cycles
        DEPTH                : integer          := 4096;
        -- Width of AXI stream interfaces in bits
        DATA_WIDTH           : integer          := 8;
        -- Propagate tkeep signal
        -- If disabled; tkeep assumed to be '1'
        KEEP_ENABLE          : integer          := ternary(DATA_WIDTH > 8, 1, 0);
        -- tkeep signal width (words per cycle)
        KEEP_WIDTH           : integer          := ((DATA_WIDTH + 7)/8);
        -- Propagate tlast signal
        LAST_ENABLE          : integer          := 1;
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
        -- number of RAM pipeline registers
        RAM_PIPELINE         : integer          := 1;
        -- use output FIFO
        -- When set; the RAM read enable and pipeline clock enables are removed
        OUTPUT_FIFO_ENABLE   : integer          := 0;
        -- Frame FIFO mode - operate on frames instead of cycles
        -- When set; m_axis_tvalid will not be deasserted within a frame
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
        -- When set; s_axis_tready is always asserted
        -- Requires FRAME_FIFO and DROP_OVERSIZE_FRAME set
        DROP_WHEN_FULL       : integer          := 0;
        -- Mark incoming frames as bad frames when full
        -- When set; s_axis_tready is always asserted
        -- Requires FRAME_FIFO to be clear
        MARK_WHEN_FULL       : integer          := 0;
        -- Enable pause request input
        PAUSE_ENABLE         : integer          := 0;
        -- Pause between frames
        FRAME_PAUSE          : integer          := FRAME_FIFO
    );
    port (
        signal clk                 : in  std_logic;
        signal rst                 : in  std_logic;

        --
        -- AXI input
        --
        signal s_axis_tdata        : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
        signal s_axis_tkeep        : in  std_logic_vector(KEEP_WIDTH - 1 downto 0);
        signal s_axis_tvalid       : in  std_logic;
        signal s_axis_tready       : out std_logic;
        signal s_axis_tlast        : in  std_logic;
        signal s_axis_tid          : in  std_logic_vector(ID_WIDTH - 1 downto 0);
        signal s_axis_tdest        : in  std_logic_vector(DEST_WIDTH - 1 downto 0);
        signal s_axis_tuser        : in  std_logic_vector(USER_WIDTH - 1 downto 0);

        --
        -- AXI output
        --
        signal m_axis_tdata        : out std_logic_vector(DATA_WIDTH - 1 downto 0);
        signal m_axis_tkeep        : out std_logic_vector(KEEP_WIDTH - 1 downto 0);
        signal m_axis_tvalid       : out std_logic;
        signal m_axis_tready       : in  std_logic;
        signal m_axis_tlast        : out std_logic;
        signal m_axis_tid          : out std_logic_vector(ID_WIDTH - 1 downto 0);
        signal m_axis_tdest        : out std_logic_vector(DEST_WIDTH - 1 downto 0);
        signal m_axis_tuser        : out std_logic_vector(USER_WIDTH - 1 downto 0);

        --
        -- Pause
        --
        signal pause_req           : in  std_logic := '0';
        signal pause_ack           : out std_logic;

        --
        -- Status
        --
        signal status_depth        : out std_logic_vector(clog2(DEPTH) downto 0);
        signal status_depth_commit : out std_logic_vector(clog2(DEPTH) downto 0);
        signal status_overflow     : out std_logic;
        signal status_bad_frame    : out std_logic;
        signal status_good_frame   : out std_logic
    );
end entity;

architecture rtl of axis_fifo is
    constant ADDR_WIDTH : integer := ternary(KEEP_ENABLE = 1 and KEEP_WIDTH > 1,
                                     clog2(DEPTH/KEEP_WIDTH),
                                     clog2(DEPTH));

    constant OUTPUT_FIFO_ADDR_WIDTH : integer := ternary(RAM_PIPELINE < 2,
                                                 3,
                                                 clog2(RAM_PIPELINE * 2 + 7));

    constant KEEP_OFFSET     : integer                       := DATA_WIDTH;
    constant LAST_OFFSET     : integer                       := KEEP_OFFSET + ternary(KEEP_ENABLE, KEEP_WIDTH, 0);
    constant ID_OFFSET       : integer                       := LAST_OFFSET + ternary(LAST_ENABLE, 1, 0);
    constant DEST_OFFSET     : integer                       := ID_OFFSET + ternary(ID_ENABLE, ID_WIDTH, 0);
    constant USER_OFFSET     : integer                       := DEST_OFFSET + ternary(DEST_ENABLE, DEST_WIDTH, 0);
    constant WIDTH           : integer                       := USER_OFFSET + ternary(USER_ENABLE, USER_WIDTH, 0);

    signal wr_ptr_reg        : unsigned(ADDR_WIDTH downto 0) := (others => '0');
    signal wr_ptr_commit_reg : unsigned(ADDR_WIDTH downto 0) := (others => '0');
    signal rd_ptr_reg        : unsigned(ADDR_WIDTH downto 0) := (others => '0');

    type t_mem is array (natural range <>) of std_logic_vector(WIDTH - 1 downto 0);

    signal mem                                 : t_mem((2 ** ADDR_WIDTH) - 1 downto 0);
    attribute ramstyle                         : string;
    attribute ramstyle of mem                  : signal is "no_rw_check";

    signal mem_read_data_valid_reg             : std_logic := '0';

    signal m_axis_pipe_reg                     : t_mem(RAM_PIPELINE + 1 - 1 downto 0);
    attribute shreg_extract                    : string;
    attribute shreg_extract of m_axis_pipe_reg : signal is "no";

    signal m_axis_tvalid_pipe_reg              : std_logic_vector(RAM_PIPELINE + 1 - 1 downto 0);

    signal full                                : std_logic;
    signal empty                               : std_logic;
    signal full_wr                             : std_logic;

    signal s_frame_reg                         : std_logic                             := '0';

    signal drop_frame_reg                      : std_logic                             := '0';
    signal mark_frame_reg                      : std_logic                             := '0';
    signal send_frame_reg                      : std_logic                             := '0';
    signal depth_reg                           : std_logic_vector(ADDR_WIDTH downto 0) := (others => '0');
    signal depth_commit_reg                    : std_logic_vector(ADDR_WIDTH downto 0) := (others => '0');
    signal overflow_reg                        : std_logic                             := '0';
    signal bad_frame_reg                       : std_logic                             := '0';
    signal good_frame_reg                      : std_logic                             := '0';

    signal s_axis                              : std_logic_vector(WIDTH - 1 downto 0);

    signal m_axis                              : std_logic_vector(WIDTH - 1 downto 0);
    signal m_axis_tready_pipe                  : std_logic;
    signal m_axis_tvalid_pipe                  : std_logic;
    signal m_axis_tdata_pipe                   : std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal m_axis_tkeep_pipe                   : std_logic_vector(KEEP_WIDTH - 1 downto 0) := (others => '1');
    signal m_axis_tlast_pipe                   : std_logic                                 := '1';
    signal m_axis_tid_pipe                     : std_logic_vector(ID_WIDTH - 1 downto 0)   := (others => '0');
    signal m_axis_tdest_pipe                   : std_logic_vector(DEST_WIDTH - 1 downto 0) := (others => '0');
    signal m_axis_tuser_pipe                   : std_logic_vector(USER_WIDTH - 1 downto 0) := (others => '0');

    signal m_axis_tready_out                   : std_logic;
    signal m_axis_tvalid_out                   : std_logic;
    signal m_axis_tdata_out                    : std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal m_axis_tkeep_out                    : std_logic_vector(KEEP_WIDTH - 1 downto 0);
    signal m_axis_tlast_out                    : std_logic;
    signal m_axis_tid_out                      : std_logic_vector(ID_WIDTH - 1 downto 0);
    signal m_axis_tdest_out                    : std_logic_vector(DEST_WIDTH - 1 downto 0);
    signal m_axis_tuser_out                    : std_logic_vector(USER_WIDTH - 1 downto 0);

    signal pipe_ready                          : std_logic;

    -- output datapath logic
    signal m_axis_tdata_reg                    : std_logic_vector(DATA_WIDTH - 1 downto 0)         := (others => '0');
    signal m_axis_tkeep_reg                    : std_logic_vector(KEEP_WIDTH - 1 downto 0)         := (others => '0');
    signal m_axis_tvalid_reg                   : std_logic                                         := '0';
    signal m_axis_tlast_reg                    : std_logic                                         := '0';
    signal m_axis_tid_reg                      : std_logic_vector(ID_WIDTH - 1 downto 0)           := (others => '0');
    signal m_axis_tdest_reg                    : std_logic_vector(DEST_WIDTH - 1 downto 0)         := (others => '0');
    signal m_axis_tuser_reg                    : std_logic_vector(USER_WIDTH - 1 downto 0)         := (others => '0');

    signal out_fifo_wr_ptr_reg                 : unsigned(OUTPUT_FIFO_ADDR_WIDTH + 1 - 1 downto 0) := (others => '0');
    signal out_fifo_rd_ptr_reg                 : unsigned(OUTPUT_FIFO_ADDR_WIDTH + 1 - 1 downto 0) := (others => '0');
    signal out_fifo_half_full_reg              : std_logic                                         := '0';

    signal out_fifo_full                       : std_logic;
    signal out_fifo_empty                      : std_logic;

    type t_out_fifo is record
        tdata : std_logic_vector(DATA_WIDTH - 1 downto 0);
        tkeep : std_logic_vector(KEEP_WIDTH - 1 downto 0);
        tlast : std_logic;
        tid   : std_logic_vector(ID_WIDTH - 1 downto 0);
        tdest : std_logic_vector(DEST_WIDTH - 1 downto 0);
        tuser : std_logic_vector(USER_WIDTH - 1 downto 0);
    end record;

    type a_out_fifo is array (natural range <>) of t_out_fifo;

    signal out_fifo                 : a_out_fifo(2 ** OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0);
    attribute ram_style             : string;
    attribute ram_style of out_fifo : signal is "distributed";
    attribute ramstyle of out_fifo  : signal is "no_rw_check, mlab";

    -- Pause logic
    signal pause_reg                : std_logic := '0';
    signal pause_frame_reg          : std_logic := '0';
begin

    -- check configuration
    P_check : process (all) is
        constant user_0 : std_logic_vector(USER_WIDTH - 1 downto 0) := (others => '0');
        constant user_1 : std_logic_vector(USER_WIDTH - 1 downto 0) := (others => '0');
    begin
        if FRAME_FIFO = 1 and LAST_ENABLE = 0 then
            report "Error: FRAME_FIFO set requires LAST_ENABLE set"
                severity failure;
        end if;

        if DROP_OVERSIZE_FRAME = 1 and FRAME_FIFO = 0 then
            report "Error: DROP_OVERSIZE_FRAME set requires FRAME_FIFO set"
                severity failure;
        end if;

        if DROP_BAD_FRAME = 1 and (FRAME_FIFO = 0 or DROP_OVERSIZE_FRAME = 0) then
            report "Error: DROP_BAD_FRAME set requires FRAME_FIFO and DROP_OVERSIZE_FRAME set"
                severity failure;
        end if;

        if DROP_WHEN_FULL = 1 and (FRAME_FIFO = 0 or DROP_OVERSIZE_FRAME = 0) then
            report "Error: DROP_WHEN_FULL set requires FRAME_FIFO and DROP_OVERSIZE_FRAME set"
                severity failure;
        end if;

        if DROP_BAD_FRAME = 1 or MARK_WHEN_FULL = 1 then
            if unsigned(USER_BAD_FRAME_MASK and const_1(USER_WIDTH)) = 0 then
                report "Error: Invalid USER_BAD_FRAME_MASK value"
                    severity failure;
            end if;
        end if;

        if MARK_WHEN_FULL = 1 and FRAME_FIFO = 1 then
            report "Error: MARK_WHEN_FULL is not compatible with FRAME_FIFO"
                severity failure;
        end if;

        if MARK_WHEN_FULL = 1 and LAST_ENABLE = 0 then
            report "Error: MARK_WHEN_FULL set requires LAST_ENABLE set"
                severity failure;
        end if;

        report "Configuration" & LF &
            "DEPTH                " & to_string(DEPTH) & LF &
            "DATA_WIDTH           " & to_string(DATA_WIDTH) & LF &
            "KEEP_ENABLE          " & to_string(KEEP_ENABLE) & LF &
            "KEEP_WIDTH           " & to_string(KEEP_WIDTH) & LF &
            "LAST_ENABLE          " & to_string(LAST_ENABLE) & LF &
            "ID_ENABLE            " & to_string(ID_ENABLE) & LF &
            "ID_WIDTH             " & to_string(ID_WIDTH) & LF &
            "DEST_ENABLE          " & to_string(DEST_ENABLE) & LF &
            "DEST_WIDTH           " & to_string(DEST_WIDTH) & LF &
            "USER_ENABLE          " & to_string(USER_ENABLE) & LF &
            "USER_WIDTH           " & to_string(USER_WIDTH) & LF &
            "RAM_PIPELINE         " & to_string(RAM_PIPELINE) & LF &
            "OUTPUT_FIFO_ENABLE   " & to_string(OUTPUT_FIFO_ENABLE) & LF &
            "FRAME_FIFO           " & to_string(FRAME_FIFO) & LF &
            "USER_BAD_FRAME_VALUE " & to_string(USER_BAD_FRAME_VALUE) & LF &
            "USER_BAD_FRAME_MASK  " & to_string(USER_BAD_FRAME_MASK) & LF &
            "DROP_OVERSIZE_FRAME  " & to_string(DROP_OVERSIZE_FRAME) & LF &
            "DROP_BAD_FRAME       " & to_string(DROP_BAD_FRAME) & LF &
            "DROP_WHEN_FULL       " & to_string(DROP_WHEN_FULL) & LF &
            "MARK_WHEN_FULL       " & to_string(MARK_WHEN_FULL) & LF &
            "PAUSE_ENABLE         " & to_string(PAUSE_ENABLE) & LF &
            "FRAME_PAUSE          " & to_string(FRAME_PAUSE);
    end process;

    -- full when first MSB different but rest same
    full <= '1' when wr_ptr_reg(ADDR_WIDTH) /= rd_ptr_reg(ADDR_WIDTH) and wr_ptr_reg(ADDR_WIDTH - 1 downto 0) = rd_ptr_reg(ADDR_WIDTH - 1 downto 0) else
            '0';

    -- empty when pointers match exactly
    empty <= '1' when wr_ptr_commit_reg = rd_ptr_reg else
             '0';

    -- overflow within packet
    full_wr <= '1' when wr_ptr_reg(ADDR_WIDTH) /= wr_ptr_commit_reg(ADDR_WIDTH) and wr_ptr_reg(ADDR_WIDTH - 1 downto 0) = wr_ptr_commit_reg(ADDR_WIDTH - 1 downto 0) else
               '0';

    s_axis_tready <= '1' when FRAME_FIFO = 1 and (full = '0' or (full_wr = '1' and DROP_OVERSIZE_FRAME = 1) or DROP_WHEN_FULL = 1) else
                     '1' when FRAME_FIFO = 0 and (full = '0' or MARK_WHEN_FULL = 1) else
                     '0';

    s_axis(DATA_WIDTH - 1 downto 0) <= s_axis_tdata;

    gkeeps : if KEEP_ENABLE = 1 generate
        s_axis(KEEP_OFFSET + KEEP_WIDTH - 1 downto KEEP_OFFSET) <= s_axis_tkeep;
    end generate;

    glasts : if LAST_ENABLE = 1 generate
        s_axis(LAST_OFFSET) <= s_axis_tlast or mark_frame_reg;
    end generate;

    gids : if ID_ENABLE = 1 generate
        s_axis(ID_OFFSET + ID_WIDTH - 1 downto ID_OFFSET) <= s_axis_tid;
    end generate;

    gdests : if DEST_ENABLE = 1 generate
        s_axis(DEST_OFFSET + DEST_WIDTH - 1 downto DEST_OFFSET) <= s_axis_tdest;
    end generate;

    gusrs : if USER_ENABLE = 1 generate
        s_axis(USER_OFFSET + USER_WIDTH - 1 downto USER_OFFSET) <= ternary(mark_frame_reg = '1', USER_BAD_FRAME_VALUE, s_axis_tuser);
    end generate;

    m_axis             <= m_axis_pipe_reg(RAM_PIPELINE + 1 - 1);
    m_axis_tdata_pipe  <= m_axis(DATA_WIDTH - 1 downto 0);
    m_axis_tvalid_pipe <= m_axis_tvalid_pipe_reg(RAM_PIPELINE + 1 - 1);

    gkeepm : if KEEP_ENABLE = 1 generate
        m_axis_tkeep_pipe <= m_axis(KEEP_OFFSET + KEEP_WIDTH - 1 downto KEEP_OFFSET);
    end generate;

    glastm : if LAST_ENABLE = 1 generate
        m_axis_tlast_pipe <= m_axis(LAST_OFFSET);
    end generate;

    gidm : if ID_ENABLE = 1 generate
        m_axis_tid_pipe <= m_axis(ID_OFFSET + ID_WIDTH - 1 downto ID_OFFSET);
    end generate;

    gdestm : if DEST_ENABLE = 1 generate
        m_axis_tdest_pipe <= m_axis(DEST_OFFSET + DEST_WIDTH - 1 downto DEST_OFFSET);
    end generate;

    gusrm : if USER_ENABLE = 1 generate
        m_axis_tuser_pipe <= m_axis(USER_OFFSET + USER_WIDTH - 1 downto USER_OFFSET);
    end generate;

    status_depth        <= ternary(KEEP_ENABLE = 1 and KEEP_WIDTH > 1, depth_reg & const_0(clog2(KEEP_WIDTH)), depth_reg);
    status_depth_commit <= ternary(KEEP_ENABLE = 1 and KEEP_WIDTH > 1, depth_commit_reg & const_0(clog2(KEEP_WIDTH)), depth_commit_reg);
    status_overflow     <= overflow_reg;
    status_bad_frame    <= bad_frame_reg;
    status_good_frame   <= good_frame_reg;

    -- Write logic
    P_write : process (clk, rst) begin
        if rst = '1' then
            wr_ptr_reg        <= (others => '0');
            wr_ptr_commit_reg <= (others => '0');

            s_frame_reg       <= '0';

            drop_frame_reg    <= '0';
            mark_frame_reg    <= '0';
            send_frame_reg    <= '0';
            overflow_reg      <= '0';
            bad_frame_reg     <= '0';
            good_frame_reg    <= '0';
        elsif rising_edge(clk) then

            overflow_reg   <= '0';
            bad_frame_reg  <= '0';
            good_frame_reg <= '0';

            if s_axis_tready = '1' and s_axis_tvalid = '1' and LAST_ENABLE = 1 then
                -- track input frame status
                s_frame_reg <= not(s_axis_tlast);
            end if;

            if FRAME_FIFO = 1 then
                -- frame FIFO mode
                if s_axis_tready = '1' and s_axis_tvalid = '1' then
                    -- transfer in
                    if ((full = '1' and DROP_WHEN_FULL = 1) or (full_wr = '1' and DROP_OVERSIZE_FRAME = 1) or drop_frame_reg = '1') then
                        -- full, packet overflow, or currently dropping frame
                        -- drop frame
                        drop_frame_reg <= '1';
                        if s_axis_tlast = '1' then
                            -- end of frame, reset write pointer
                            wr_ptr_reg     <= wr_ptr_commit_reg;
                            drop_frame_reg <= '0';
                            overflow_reg   <= '1';
                        end if;
                    else
                        -- store it
                        mem(to_integer(wr_ptr_reg(ADDR_WIDTH - 1 downto 0))) <= s_axis;
                        wr_ptr_reg                                           <= wr_ptr_reg + 1;
                        if (s_axis_tlast = '1' or (DROP_OVERSIZE_FRAME = 0 and (full_wr = '1' or send_frame_reg = '1'))) then
                            -- end of frame or send frame
                            send_frame_reg <= not(s_axis_tlast);
                            if s_axis_tlast = '1' and DROP_BAD_FRAME = 1 and mask_check(s_axis_tuser, USER_BAD_FRAME_MASK, USER_BAD_FRAME_VALUE) then
                                -- bad packet, reset write pointer
                                wr_ptr_reg    <= wr_ptr_commit_reg;
                                bad_frame_reg <= '1';
                            else
                                -- good packet or packet overflow, update write pointer
                                wr_ptr_commit_reg <= wr_ptr_reg + 1;
                                good_frame_reg    <= s_axis_tlast;
                            end if;
                        end if;
                    end if;
                elsif (s_axis_tvalid = '1' and full_wr = '1' and DROP_OVERSIZE_FRAME = 0) then
                    -- data valid with packet overflow
                    -- update write pointer
                    send_frame_reg    <= '1';
                    wr_ptr_commit_reg <= wr_ptr_reg;
                end if;
            else
                -- normal FIFO mode
                if s_axis_tready = '1' and s_axis_tvalid = '1' then
                    if drop_frame_reg = '1' and MARK_WHEN_FULL = 1 then
                        -- currently dropping frame
                        if s_axis_tlast = '1' then
                            -- end of frame
                            if full = '0' and mark_frame_reg = '1' then
                                -- terminate marked frame
                                mark_frame_reg                                       <= '0';
                                mem(to_integer(wr_ptr_reg(ADDR_WIDTH - 1 downto 0))) <= s_axis;
                                wr_ptr_reg                                           <= wr_ptr_reg + 1;
                                wr_ptr_commit_reg                                    <= wr_ptr_reg + 1;
                            end if;
                            -- end of frame, clear drop flag
                            drop_frame_reg <= '0';
                            overflow_reg   <= '1';
                        end if;
                    elsif (full = '1' or mark_frame_reg = '1') and MARK_WHEN_FULL = 1 then
                        -- full or marking frame
                        -- drop frame; mark if this isn't the first cycle
                        drop_frame_reg <= '1';
                        mark_frame_reg <= mark_frame_reg or s_frame_reg;
                        if s_axis_tlast = '1' then
                            drop_frame_reg <= '0';
                            overflow_reg   <= '1';
                        end if;
                    else
                        -- transfer in
                        mem(to_integer(wr_ptr_reg(ADDR_WIDTH - 1 downto 0))) <= s_axis;
                        wr_ptr_reg                                           <= wr_ptr_reg + 1;
                        wr_ptr_commit_reg                                    <= wr_ptr_reg + 1;
                    end if;
                elsif ((full = '0' and drop_frame_reg = '0' and mark_frame_reg = '1') and MARK_WHEN_FULL = 1) then
                    -- terminate marked frame
                    mark_frame_reg                                       <= '0';
                    mem(to_integer(wr_ptr_reg(ADDR_WIDTH - 1 downto 0))) <= s_axis;
                    wr_ptr_reg                                           <= wr_ptr_reg + 1;
                    wr_ptr_commit_reg                                    <= wr_ptr_reg + 1;
                end if;
            end if;
        end if;
    end process;

    -- Status
    P_status : process (clk) begin
        if rising_edge(clk) then
            depth_reg        <= std_logic_vector(wr_ptr_reg - rd_ptr_reg);
            depth_commit_reg <= std_logic_vector(wr_ptr_commit_reg - rd_ptr_reg);
        end if;
    end process;

    -- Read logic
    P_read : process (clk, rst) begin
        if rst = '1' then
            rd_ptr_reg             <= (others => '0');
            m_axis_tvalid_pipe_reg <= (others => '0');
        elsif rising_edge(clk) then
            if m_axis_tready_pipe = '1' then
                -- output ready; invalidate stage
                m_axis_tvalid_pipe_reg(RAM_PIPELINE + 1 - 1) <= '0';
            end if;

            for j in RAM_PIPELINE + 1 - 1 downto 1 loop
                -- if (m_axis_tready_pipe || ((~m_axis_tvalid_pipe_reg) >> j)) begin
                if m_axis_tready_pipe = '1' or (unsigned(not(m_axis_tvalid_pipe_reg)) srl j) > 0 then
                    -- output ready or bubble in pipeline; transfer down pipeline
                    m_axis_tvalid_pipe_reg(j)     <= m_axis_tvalid_pipe_reg(j - 1);
                    m_axis_pipe_reg(j)            <= m_axis_pipe_reg(j - 1);
                    m_axis_tvalid_pipe_reg(j - 1) <= '0';
                end if;
            end loop;

            -- if (m_axis_tready_pipe || ~m_axis_tvalid_pipe_reg) begin
            if m_axis_tready_pipe = '1' or unsigned(not(m_axis_tvalid_pipe_reg)) > 0  then
                -- output ready or bubble in pipeline; read new data from FIFO
                m_axis_tvalid_pipe_reg(0) <= '0';
                m_axis_pipe_reg(0)        <= mem(to_integer(rd_ptr_reg(ADDR_WIDTH - 1 downto 0)));
                if empty = '0' and pipe_ready = '1' then
                    -- not empty, increment pointer
                    m_axis_tvalid_pipe_reg(0) <= '1';
                    rd_ptr_reg                <= rd_ptr_reg + 1;
                end if;
            end if;
        end if;
    end process;

    no_fifo : if OUTPUT_FIFO_ENABLE = 0 generate
        pipe_ready         <= '1';

        m_axis_tready_pipe <= m_axis_tready_out;
        m_axis_tvalid_out  <= m_axis_tvalid_pipe;

        m_axis_tdata_out   <= m_axis_tdata_pipe;
        m_axis_tkeep_out   <= m_axis_tkeep_pipe;
        m_axis_tlast_out   <= m_axis_tlast_pipe;
        m_axis_tid_out     <= m_axis_tid_pipe;
        m_axis_tdest_out   <= m_axis_tdest_pipe;
        m_axis_tuser_out   <= m_axis_tuser_pipe;
    end generate;

    output_fifo : if OUTPUT_FIFO_ENABLE = 1 generate

        -- output datapath logic
        out_fifo_full <= '1' when out_fifo_wr_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH) /= out_fifo_rd_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH) and out_fifo_wr_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0) = out_fifo_rd_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0) else
                         '0';
        out_fifo_empty <= '1' when out_fifo_wr_ptr_reg = out_fifo_rd_ptr_reg else
                          '0';

        pipe_ready         <= not(out_fifo_half_full_reg);

        m_axis_tready_pipe <= '1';

        m_axis_tdata_out   <= m_axis_tdata_reg;
        m_axis_tkeep_out   <= ternary(KEEP_ENABLE, m_axis_tkeep_reg, const_1(KEEP_WIDTH));
        m_axis_tvalid_out  <= m_axis_tvalid_reg;
        m_axis_tlast_out   <= ternary(LAST_ENABLE, m_axis_tlast_reg, '1');
        m_axis_tid_out     <= ternary(ID_ENABLE, m_axis_tid_reg, const_0(ID_WIDTH));
        m_axis_tdest_out   <= ternary(DEST_ENABLE, m_axis_tdest_reg, const_0(DEST_WIDTH));
        m_axis_tuser_out   <= ternary(USER_ENABLE, m_axis_tuser_reg, const_0(USER_WIDTH));

        P_out_fifo : process (clk, rst) begin
            if rst = '1' then
                out_fifo_wr_ptr_reg <= (others => '0');
                out_fifo_rd_ptr_reg <= (others => '0');
                m_axis_tvalid_reg   <= '0';
            elsif rising_edge(clk) then
                m_axis_tvalid_reg      <= m_axis_tvalid_reg and not m_axis_tready_out;

                out_fifo_half_full_reg <= '1' when unsigned(out_fifo_wr_ptr_reg - out_fifo_rd_ptr_reg) >= 2 ** (OUTPUT_FIFO_ADDR_WIDTH - 1) else
                                          '0';

                if out_fifo_full = '0' and m_axis_tvalid_pipe = '1'then
                    out_fifo(to_integer(out_fifo_wr_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0))).tdata <= m_axis_tdata_pipe;
                    out_fifo(to_integer(out_fifo_wr_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0))).tkeep <= m_axis_tkeep_pipe;
                    out_fifo(to_integer(out_fifo_wr_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0))).tlast <= m_axis_tlast_pipe;
                    out_fifo(to_integer(out_fifo_wr_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0))).tid   <= m_axis_tid_pipe;
                    out_fifo(to_integer(out_fifo_wr_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0))).tdest <= m_axis_tdest_pipe;
                    out_fifo(to_integer(out_fifo_wr_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0))).tuser <= m_axis_tuser_pipe;
                    out_fifo_wr_ptr_reg                                                                  <= out_fifo_wr_ptr_reg + 1;
                end if;

                if out_fifo_empty = '0' and (m_axis_tvalid_reg = '0' or m_axis_tready_out = '1') then
                    m_axis_tdata_reg    <= out_fifo(to_integer(out_fifo_rd_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0))).tdata;
                    m_axis_tkeep_reg    <= out_fifo(to_integer(out_fifo_rd_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0))).tkeep;
                    m_axis_tvalid_reg   <= '1';
                    m_axis_tlast_reg    <= out_fifo(to_integer(out_fifo_rd_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0))).tlast;
                    m_axis_tid_reg      <= out_fifo(to_integer(out_fifo_rd_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0))).tid;
                    m_axis_tdest_reg    <= out_fifo(to_integer(out_fifo_rd_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0))).tdest;
                    m_axis_tuser_reg    <= out_fifo(to_integer(out_fifo_rd_ptr_reg(OUTPUT_FIFO_ADDR_WIDTH - 1 downto 0))).tuser;
                    out_fifo_rd_ptr_reg <= out_fifo_rd_ptr_reg + 1;
                end if;
            end if;

        end process;

    end generate;

    pause : if PAUSE_ENABLE = 1 generate

        -- Pause logic
        m_axis_tready_out <= m_axis_tready and not(pause_reg);
        m_axis_tvalid     <= m_axis_tvalid_out and not(pause_reg);

        m_axis_tdata      <= m_axis_tdata_out;
        m_axis_tkeep      <= m_axis_tkeep_out;
        m_axis_tlast      <= m_axis_tlast_out;
        m_axis_tid        <= m_axis_tid_out;
        m_axis_tdest      <= m_axis_tdest_out;
        m_axis_tuser      <= m_axis_tuser_out;

        pause_ack         <= pause_reg;

        p_pause : process (clk, rst) begin
            if rst = '1' then
                pause_frame_reg <= '0';
                pause_reg       <= '0';
            elsif rising_edge(clk) then
                if FRAME_PAUSE = 1 then
                    if pause_reg = '1' then
                        -- paused; update pause status
                        pause_reg <= pause_req;
                    elsif m_axis_tvalid_out = '1' then
                        -- frame transfer; set frame bit
                        pause_frame_reg <= '1';
                        if m_axis_tready = '1' and m_axis_tlast = '1' then
                            -- end of frame; clear frame bit and update pause status
                            pause_frame_reg <= '0';
                            pause_reg       <= pause_req;
                        end if;
                    elsif pause_frame_reg = '0' then
                        -- idle; update pause status
                        pause_reg <= pause_req;
                    end if;
                else
                    pause_reg <= pause_req;
                end if;
            end if;
        end process;
    end generate;

    no_pause : if PAUSE_ENABLE = 0 generate
        m_axis_tready_out <= m_axis_tready;
        m_axis_tvalid     <= m_axis_tvalid_out;

        m_axis_tdata      <= m_axis_tdata_out;
        m_axis_tkeep      <= m_axis_tkeep_out;
        m_axis_tlast      <= m_axis_tlast_out;
        m_axis_tid        <= m_axis_tid_out;
        m_axis_tdest      <= m_axis_tdest_out;
        m_axis_tuser      <= m_axis_tuser_out;

        pause_ack         <= '0';

    end generate;

end architecture;