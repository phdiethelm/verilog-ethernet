-- 
-- Copyright (c) 2014-2023 Alex Forencich
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
--  AXI4-Stream bus width adapter
-- 
entity axis_adapter is
    generic (
        -- Width of input AXI stream interface in bits
        S_DATA_WIDTH  : integer := 8;
        -- Propagate tkeep signal on input interface
        -- If disabled; tkeep assumed to be '1'
        S_KEEP_ENABLE : integer := ternary(S_DATA_WIDTH > 8, 1, 0);
        -- tkeep signal width (words per cycle) on input interface
        S_KEEP_WIDTH  : integer := ((S_DATA_WIDTH + 7)/8);
        -- Width of output AXI stream interface in bits
        M_DATA_WIDTH  : integer := 8;
        -- Propagate tkeep signal on output interface
        -- If disabled; tkeep assumed to be '1'
        M_KEEP_ENABLE : integer := ternary(M_DATA_WIDTH > 8, 1, 0);
        -- tkeep signal width (words per cycle) on output interface
        M_KEEP_WIDTH  : integer := ((M_DATA_WIDTH + 7)/8);
        -- Propagate tid signal
        ID_ENABLE     : integer := 0;
        -- tid signal width
        ID_WIDTH      : integer := 8;
        -- Propagate tdest signal
        DEST_ENABLE   : integer := 0;
        -- tdest signal width
        DEST_WIDTH    : integer := 8;
        -- Propagate tuser signal
        USER_ENABLE   : integer := 1;
        -- tuser signal width
        USER_WIDTH    : integer := 1
    );
    port (
        signal clk           : in  std_logic;
        signal rst           : in  std_logic;

        -- 
        -- AXI input
        -- 
        signal s_axis_tdata  : in  std_logic_vector(S_DATA_WIDTH - 1 downto 0);
        signal s_axis_tkeep  : in  std_logic_vector(S_KEEP_WIDTH - 1 downto 0);
        signal s_axis_tvalid : in  std_logic;
        signal s_axis_tready : out std_logic;
        signal s_axis_tlast  : in  std_logic;
        signal s_axis_tid    : in  std_logic_vector(ID_WIDTH - 1 downto 0);
        signal s_axis_tdest  : in  std_logic_vector(DEST_WIDTH - 1 downto 0);
        signal s_axis_tuser  : in  std_logic_vector(USER_WIDTH - 1 downto 0);

        --
        -- AXI output
        --
        signal m_axis_tdata  : out std_logic_vector(M_DATA_WIDTH - 1 downto 0);
        signal m_axis_tkeep  : out std_logic_vector(M_KEEP_WIDTH - 1 downto 0);
        signal m_axis_tvalid : out std_logic;
        signal m_axis_tready : in  std_logic;
        signal m_axis_tlast  : out std_logic;
        signal m_axis_tid    : out std_logic_vector(ID_WIDTH - 1 downto 0);
        signal m_axis_tdest  : out std_logic_vector(DEST_WIDTH - 1 downto 0);
        signal m_axis_tuser  : out std_logic_vector(USER_WIDTH - 1 downto 0)
    );
end entity;

architecture rtl of axis_adapter is
    -- force keep width to 1 when disabled
    constant S_BYTE_LANES    : integer := ternary(S_KEEP_ENABLE, S_KEEP_WIDTH, 1);
    constant M_BYTE_LANES    : integer := ternary(M_KEEP_ENABLE, M_KEEP_WIDTH, 1);

    -- bus byte sizes (must be identical)
    constant S_BYTE_SIZE     : integer := S_DATA_WIDTH / S_BYTE_LANES;
    constant M_BYTE_SIZE     : integer := M_DATA_WIDTH / M_BYTE_LANES;

    signal SEG_COUNT         : integer;
    signal SEG_DATA_WIDTH    : integer;
    signal SEG_KEEP_WIDTH    : integer;
    signal seg_reg           : integer;

    signal s_axis_tdata_reg  : std_logic_vector(S_DATA_WIDTH - 1 downto 0) := (others => '0');
    signal s_axis_tkeep_reg  : std_logic_vector(S_KEEP_WIDTH - 1 downto 0) := (others => '0');
    signal s_axis_tvalid_reg : std_logic                                   := '0';
    signal s_axis_tlast_reg  : std_logic                                   := '0';
    signal s_axis_tid_reg    : std_logic_vector(ID_WIDTH - 1 downto 0)     := (others => '0');
    signal s_axis_tdest_reg  : std_logic_vector(DEST_WIDTH - 1 downto 0)   := (others => '0');
    signal s_axis_tuser_reg  : std_logic_vector(USER_WIDTH - 1 downto 0)   := (others => '0');

    signal m_axis_tdata_reg  : std_logic_vector(M_DATA_WIDTH - 1 downto 0) := (others => '0');
    signal m_axis_tkeep_reg  : std_logic_vector(M_KEEP_WIDTH - 1 downto 0) := (others => '0');
    signal m_axis_tvalid_reg : std_logic                                   := '0';
    signal m_axis_tlast_reg  : std_logic                                   := '0';
    signal m_axis_tid_reg    : std_logic_vector(ID_WIDTH - 1 downto 0)     := (others => '0');
    signal m_axis_tdest_reg  : std_logic_vector(DEST_WIDTH - 1 downto 0)   := (others => '0');
    signal m_axis_tuser_reg  : std_logic_vector(USER_WIDTH - 1 downto 0)   := (others => '0');

begin

    -- bus width assertions
    P_check : process (all) begin
        if S_BYTE_SIZE * S_BYTE_LANES /= S_DATA_WIDTH then
            report "Error: input data width not evenly divisible"
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

    g_bypass : if M_BYTE_LANES = S_BYTE_LANES generate
        -- same width; bypass

        s_axis_tready <= m_axis_tready;

        m_axis_tdata  <= s_axis_tdata;
        m_axis_tkeep  <= ternary(M_KEEP_ENABLE, s_axis_tkeep, const_1(M_KEEP_WIDTH));
        m_axis_tvalid <= s_axis_tvalid;
        m_axis_tlast  <= s_axis_tlast;
        m_axis_tid    <= ternary(ID_ENABLE, s_axis_tid, const_0(ID_WIDTH));
        m_axis_tdest  <= ternary(DEST_ENABLE, s_axis_tdest, const_0(DEST_WIDTH));
        m_axis_tuser  <= ternary(USER_ENABLE, s_axis_tuser, const_0(USER_WIDTH));
    end generate;

    g_upsize : if M_BYTE_LANES > S_BYTE_LANES generate
        -- output is wider; upsize

        -- required number of segments in wider bus
        SEG_COUNT      <= M_BYTE_LANES / S_BYTE_LANES;
        -- data width and keep width per segment
        SEG_DATA_WIDTH <= M_DATA_WIDTH / SEG_COUNT;
        SEG_KEEP_WIDTH <= M_BYTE_LANES / SEG_COUNT;

        s_axis_tready  <= not s_axis_tvalid_reg;

        m_axis_tdata   <= m_axis_tdata_reg;
        m_axis_tkeep   <= ternary(M_KEEP_ENABLE, m_axis_tkeep_reg, const_1(M_KEEP_WIDTH));
        m_axis_tvalid  <= m_axis_tvalid_reg;
        m_axis_tlast   <= m_axis_tlast_reg;
        m_axis_tid     <= ternary(ID_ENABLE, m_axis_tid_reg, const_0(ID_WIDTH));
        m_axis_tdest   <= ternary(DEST_ENABLE, m_axis_tdest_reg, const_0(DEST_WIDTH));
        m_axis_tuser   <= ternary(USER_ENABLE, m_axis_tuser_reg, const_0(USER_WIDTH));

        process (clk) begin

            if rising_edge(clk) then

                m_axis_tvalid_reg <= m_axis_tvalid_reg and not m_axis_tready;

                if m_axis_tvalid_reg = '0' or m_axis_tready = '1' then
                    -- output register empty

                    if seg_reg = 0 then
                        m_axis_tdata_reg((seg_reg + 1) * SEG_DATA_WIDTH - 1 downto seg_reg * SEG_DATA_WIDTH) <= ternary(s_axis_tvalid_reg = '1', s_axis_tdata_reg, s_axis_tdata);
                        m_axis_tkeep_reg                                                                     <= ternary(s_axis_tvalid_reg = '1', s_axis_tkeep_reg, s_axis_tkeep);
                    else
                        m_axis_tdata_reg((seg_reg + 1) * SEG_DATA_WIDTH - 1 downto seg_reg * SEG_DATA_WIDTH) <= s_axis_tdata;
                        m_axis_tkeep_reg((seg_reg + 1) * SEG_KEEP_WIDTH - 1 downto seg_reg * SEG_KEEP_WIDTH) <= s_axis_tkeep;
                    end if;
                    m_axis_tlast_reg <= ternary(s_axis_tvalid_reg = '1', s_axis_tlast_reg, s_axis_tlast);
                    m_axis_tid_reg   <= ternary(s_axis_tvalid_reg = '1', s_axis_tid_reg, s_axis_tid);
                    m_axis_tdest_reg <= ternary(s_axis_tvalid_reg = '1', s_axis_tdest_reg, s_axis_tdest);
                    m_axis_tuser_reg <= ternary(s_axis_tvalid_reg = '1', s_axis_tuser_reg, s_axis_tuser);

                    if s_axis_tvalid_reg = '1' then
                        -- consume data from buffer
                        s_axis_tvalid_reg <= '0';

                        if s_axis_tlast_reg = '1' or seg_reg = SEG_COUNT - 1 then
                            seg_reg           <= 0;
                            m_axis_tvalid_reg <= '1';
                        else
                            seg_reg <= seg_reg + 1;
                        end if;
                    elsif s_axis_tvalid = '1' then
                        -- data direct from input
                        if s_axis_tlast = '1' or seg_reg = SEG_COUNT - 1 then
                            seg_reg           <= 0;
                            m_axis_tvalid_reg <= '1';
                        else
                            seg_reg <= seg_reg + 1;
                        end if;
                    end if;
                elsif s_axis_tvalid = '1' and s_axis_tready = '1' then
                    -- store input data in skid buffer
                    s_axis_tdata_reg  <= s_axis_tdata;
                    s_axis_tkeep_reg  <= s_axis_tkeep;
                    s_axis_tvalid_reg <= '1';
                    s_axis_tlast_reg  <= s_axis_tlast;
                    s_axis_tid_reg    <= s_axis_tid;
                    s_axis_tdest_reg  <= s_axis_tdest;
                    s_axis_tuser_reg  <= s_axis_tuser;
                end if;

                if rst = '1' then
                    seg_reg           <= 0;
                    s_axis_tvalid_reg <= '0';
                    m_axis_tvalid_reg <= '0';
                end if;
            end if;
        end process;
    end generate;

    g_downsize : if M_BYTE_LANES < S_BYTE_LANES generate
        -- output is narrower; downsize

        -- required number of segments in wider bus
        SEG_COUNT      <= S_BYTE_LANES / M_BYTE_LANES;
        -- data width and keep width per segment
        SEG_DATA_WIDTH <= S_DATA_WIDTH / SEG_COUNT;
        SEG_KEEP_WIDTH <= S_BYTE_LANES / SEG_COUNT;

        s_axis_tready  <= not s_axis_tvalid_reg;

        m_axis_tdata   <= m_axis_tdata_reg;
        m_axis_tkeep   <= ternary(M_KEEP_ENABLE, m_axis_tkeep_reg, const_1(M_KEEP_WIDTH));
        m_axis_tvalid  <= m_axis_tvalid_reg;
        m_axis_tlast   <= m_axis_tlast_reg;
        m_axis_tid     <= ternary(ID_ENABLE, m_axis_tid_reg, const_0(ID_WIDTH));
        m_axis_tdest   <= ternary(DEST_ENABLE, m_axis_tdest_reg, const_0(DEST_WIDTH));
        m_axis_tuser   <= ternary(USER_ENABLE, m_axis_tuser_reg, const_0(USER_WIDTH));

        process (clk) begin
            if rising_edge(clk) then
                m_axis_tvalid_reg <= m_axis_tvalid_reg and not m_axis_tready;

                if m_axis_tvalid_reg = '0' or m_axis_tready = '1' then
                    -- output register empty

                    m_axis_tdata_reg <= ternary(s_axis_tvalid_reg= '1', s_axis_tdata_reg, s_axis_tdata);
                    m_axis_tkeep_reg <= ternary(s_axis_tvalid_reg= '1', s_axis_tkeep_reg, s_axis_tkeep);
                    m_axis_tlast_reg <= '0';
                    m_axis_tid_reg   <= ternary(s_axis_tvalid_reg= '1', s_axis_tid_reg, s_axis_tid);
                    m_axis_tdest_reg <= ternary(s_axis_tvalid_reg= '1', s_axis_tdest_reg, s_axis_tdest);
                    m_axis_tuser_reg <= ternary(s_axis_tvalid_reg= '1', s_axis_tuser_reg, s_axis_tuser);

                    if s_axis_tvalid_reg = '1' then
                        -- buffer has data; shift out from buffer
                        s_axis_tdata_reg  <= s_axis_tdata_reg srl SEG_DATA_WIDTH; -- s_axis_tdata_reg >> SEG_DATA_WIDTH;
                        s_axis_tkeep_reg  <= s_axis_tkeep_reg srl SEG_KEEP_WIDTH; -- s_axis_tkeep_reg >> SEG_KEEP_WIDTH

                        m_axis_tvalid_reg <= '1';

                        if unsigned(s_axis_tkeep_reg srl SEG_KEEP_WIDTH) = 0 then
                            s_axis_tvalid_reg <= '0';
                            m_axis_tlast_reg  <= s_axis_tlast_reg;
                        end if;
                    elsif s_axis_tvalid = '1' and s_axis_tready = '1' then
                        -- buffer is empty; store from input
                        s_axis_tdata_reg  <= s_axis_tdata srl SEG_DATA_WIDTH;
                        s_axis_tkeep_reg  <= s_axis_tkeep srl SEG_KEEP_WIDTH;
                        s_axis_tlast_reg  <= s_axis_tlast;
                        s_axis_tid_reg    <= s_axis_tid;
                        s_axis_tdest_reg  <= s_axis_tdest;
                        s_axis_tuser_reg  <= s_axis_tuser;

                        m_axis_tvalid_reg <= '1';

                        if unsigned(s_axis_tkeep srl SEG_KEEP_WIDTH) = 0 then
                            s_axis_tvalid_reg <= '0';
                            m_axis_tlast_reg  <= s_axis_tlast;
                        else
                            s_axis_tvalid_reg <= '1';
                        end if;
                    end if;
                elsif s_axis_tvalid = '1' and s_axis_tready = '1' then
                    -- store input data
                    s_axis_tdata_reg  <= s_axis_tdata;
                    s_axis_tkeep_reg  <= s_axis_tkeep;
                    s_axis_tvalid_reg <= '1';
                    s_axis_tlast_reg  <= s_axis_tlast;
                    s_axis_tid_reg    <= s_axis_tid;
                    s_axis_tdest_reg  <= s_axis_tdest;
                    s_axis_tuser_reg  <= s_axis_tuser;
                end if;

                if rst = '1' then
                    s_axis_tvalid_reg <= '0';
                    m_axis_tvalid_reg <= '0';
                end if;
            end if;
        end process;
    end generate;

end architecture;