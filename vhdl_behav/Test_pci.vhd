--===================================================================--
--
--  www.OpenCores.Org - January 2000
--  This model adheres to the GNU public license  
--
-- Design units   : TestBench for PCI devices. 
--
-- File name      : Test_PCI.vhd
--
-- Purpose        : Implements the test bench for PCI 33 MHz, 32 bit devices.
--                  It is included one master device and two target devices.
--
--                There can be used more than one target devices in a
--                design, every device being identified by the three 
--                base addresses in generic.
--
--
-- Library        : PCI_Lib
--
-- Dependencies   : IEEE.Std_Logic_1164
--
-- Limitations    : None known
--
-- Errors         : None known
--
-- Author         : Ovidiu Lupas
--                  olupas@opencores.org
--
-- Simulator    : ModelSim EE version 5.2 on a Windows95 PC
--                ActiveVHDL 3.1 on a Windows95 PC
--===================================================================--
-----------------------------------------------------------------------
-- Entity for PCI bus Arbiter and CLK generator
-----------------------------------------------------------------------
library ieee,work;
   use ieee.Std_Logic_1164.all;
   use work.Simulation.all;
   use work.PCI_Def.all;
-----------------------------------------------------------------------
-----------------------------------------------------------------------
entity ClkGen is 
   generic ( tperiod : Time := 30 ns );
   port (
      REQ_N : in  Std_Logic;
      GNT_N : out Std_Logic := '1';
      RST_N : out  Std_Logic;
      CLK   : out Std_Logic);   -- System clock
end ClkGen; --=================== End of entity =====================--
--========================== Architecture ===========================--
architecture BEHAV_ClkGen of ClkGen is
begin--======================== Architecture =================================--
  ---------------------------------------------------------------------
  -- Provide the external clock signal to the processor
  ---------------------------------------------------------------------
  ClkDriver : process
    variable clktmp: std_logic := '0';
  begin
     CLK <= clktmp;
     clktmp := not clktmp;
    wait for tperiod/2;
  end process ClkDriver;
  ---------------------------------------------------------------------
  -- Simulates an external PCI-bus arbiter, which always grants the 
  -- access to the bus :)
  ---------------------------------------------------------------------
  Arbiter : process(REQ_N)
  begin
     if Falling_Edge(REQ_N) then
        GNT_N <= '0' after 10 ns;
     elsif Rising_Edge(REQ_N) then
        GNT_N <= '1' after 10 ns;
     end if;
  end process Arbiter;
  ---------------------------------------------------------------------
  -- Provides the reset signal
  ---------------------------------------------------------------------
  RstGen: process
  begin
     RST_N <= '0';
    wait for 100 ns;
     RST_N <= '1';
    wait for 400 ns;
    end process;
end BEHAV_ClkGen; --============== End of architecture ==============--
-----------------------------------------------------------------------
-- TestBench
-----------------------------------------------------------------------
library ieee,work,std;
   use ieee.std_logic_1164.all;
   use work.ClkGen;
-----------------------------------------------------------------------
-----------------------------------------------------------------------
entity TESTPCI is
end TESTPCI;
--========================== Architecture ===========================--
architecture stimulus of TESTPCI is
  ---------------------------------------------------------------------
  -- Signal declaration
  ---------------------------------------------------------------------
  signal AD_Bus   : Std_Logic_Vector (31 downto 0);
  signal C_BE_Bus : Std_Logic_Vector (3 downto 0);
  signal PAR      : Std_Logic;
  signal FRAME_N  : Std_Logic;
  signal TRDY_N   : Std_Logic;
  signal IRDY_N   : Std_Logic;
  signal STOP_N   : Std_Logic;
  signal DEVSEL_N : Std_Logic;
  signal IDSEL    : Std_Logic;
  signal SEL1     : Std_Logic;
  signal SEL2     : Std_Logic;
  signal PERR_N   : Std_Logic;
  signal SERR_N   : Std_Logic;
  signal REQ_N    : Std_Logic;
  signal GNT_N    : Std_Logic;
  signal CLK      : Std_Logic;
  signal RST_N    : Std_Logic;
  ---------------------------------------------------------------------
  -- Component declarations
  ---------------------------------------------------------------------
  component ClkGen 
     generic ( tperiod : Time := 30 ns );
     port (
        REQ_N : in  Std_Logic;
        GNT_N : out Std_Logic;
        RST_N : out  Std_Logic;
        CLK   : out Std_Logic);   -- System clock
  end component;
  ---------------------------------------------------------------------
  ---------------------------------------------------------------------
  component MS32PCI
    generic (
        cmd_file : string(1 to 7);
        tdelay   : Time;
        tsetup   : Time;
        thold    : Time);
    port (
    	-- Address, Data and Command buses (37)
        AD_Bus   : inout STD_LOGIC_VECTOR (31 downto 0);
        C_BE_Bus : inout STD_LOGIC_VECTOR (3 downto 0);
        PAR      : inout STD_LOGIC;
        -- Interface control signals (6)
        FRAME_N  : inout STD_LOGIC;
        TRDY_N   : in    STD_LOGIC;
        IRDY_N   : inout STD_LOGIC;
        STOP_N   : in    STD_LOGIC;
        DEVSEL_N : in    STD_LOGIC;
        IDSEL    : in    STD_LOGIC; -- in
        -- Error reporting signals (2)
        PERR_N   : inout STD_LOGIC;
        SERR_N   : inout STD_LOGIC;
        -- Arbitration signals (2)
        REQ_N    : out   STD_LOGIC; 
        GNT_N    : in    STD_LOGIC; -- in
        -- System signals (2)
        CLK      : in    STD_LOGIC;
        RST_N    : in    STD_LOGIC); --in
  end component;
  ---------------------------------------------------------------------
  ---------------------------------------------------------------------
  component TG32PCI
    generic (
      devtype : string(1 to 4);
      tdelay  : Time;
      tsetup  : Time;
      thold   : Time;
      bamem   : Std_Logic_Vector(31 downto 0); -- hex value
      baio    : Std_Logic_Vector(31 downto 0); -- hex value
      bacfg   : Std_Logic_Vector(31 downto 0));-- hex value
    port (
    	-- Address, Data and Command buses (37)
        AD_Bus   : inout Std_Logic_Vector (31 downto 0);
        C_BE_Bus : in    Std_Logic_Vector (3 downto 0);
        PAR      : inout Std_Logic;
        -- Interface control signals (6)
        FRAME_N  : in    Std_Logic;
        TRDY_N   : inout Std_Logic;
        IRDY_N   : in    Std_Logic;
        STOP_N   : out   Std_Logic;
        DEVSEL_N : inout Std_Logic;
        IDSEL    : in    Std_Logic;
        -- Error reporting signals (2)
        PERR_N   : inout Std_Logic;
        SERR_N   : inout Std_Logic;
        -- System signals (2)
        CLK      : in    Std_Logic;
        RST_N    : in    Std_Logic);
  end component;
begin --====================== Architecture =========================--
  ---------------------------------------------------------------------
  -- Component instantiation
  ---------------------------------------------------------------------
  UUT1: MS32PCI
       generic map (
            "PCI.CMD",0 ns, 7 ns, 5 ns)
       port map (
            AD_Bus,C_BE_Bus,PAR,FRAME_N,TRDY_N,IRDY_N,STOP_N,DEVSEL_N,
            IDSEL,PERR_N,SERR_N,REQ_N,GNT_N,CLK,RST_N);
  SEL1 <= AD_Bus(16);
  UUT2: TG32PCI 
       generic map (
            "Fast",0 ns,0 ns,0 ns,x"00005000",x"00000800",x"00010CF0")
      port map (
            AD_Bus => AD_Bus,C_BE_Bus => C_BE_Bus,PAR => PAR,
            FRAME_N => FRAME_N,TRDY_N => TRDY_N,IRDY_N => IRDY_N,
            STOP_N => STOP_N,DEVSEL_N => DEVSEL_N,IDSEL => SEL1,
            PERR_N => PERR_N,SERR_N => SERR_N,CLK => CLK,RST_N => RST_N);
  SEL2 <= AD_Bus(17);
  UUT3: TG32PCI
       generic map (
            "Medi",0 ns,7 ns,0 ns,x"00006000",x"00001800",x"00020CF0")
       port map (
            AD_Bus => AD_Bus,C_BE_Bus => C_BE_Bus,PAR => PAR,
            FRAME_N => FRAME_N,TRDY_N => TRDY_N,IRDY_N => IRDY_N,
            STOP_N => STOP_N,DEVSEL_N => DEVSEL_N,IDSEL => SEL2,
            PERR_N => PERR_N,SERR_N => SERR_N,CLK => CLK,RST_N => RST_N);
  GEN: ClkGen port map (
            REQ_N,GNT_N,RST_N,CLK);
  ---------------------------------------------------------------------
end stimulus; --=============== End of architecture =================--
-----------------------------------------------------------------------
-- Revision list
-- Version     Author          Date           Changes
--
-- 0.1       Ovidiu Lupas   June 09, 2000     New model
-----------------------------------------------------------------------

