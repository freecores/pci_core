--===================================================================--
--
--  www.OpenCores.Org - January 2000
--  This model adheres to the GNU public license  
--
-- Design units   : Target device for PCI Local Bus 33 MHz 32 bits
--                  (BoardLevel Simulation model)
--                  (Entity and architecture)
--
-- File name      : Tg32PCI.vhd
--
-- Purpose        : The Target device is used to simulate a target 
--                  device on the PCI-Bus
--
-- Note           : This model is modelled after the PCI protocol 
--                  as described in Xilinx & Altera AppNotes
--
--                There can be used more than one target devices in a
--                design, every device being identified by the three 
--                base addresses in generic.
--
-- Limitations    : None known
--
-- Errors         : None known
--
-- Library        : PCI_Lib.vhd
--
-- Dependencies   : IEEE.Std_Logic_1164
--
-- Author         : Ovidiu Lupas
--                  olupas@opencores.org
--
-- Simulator    : ModelSim EE version 5.2 on a Windows95 PC
--                ActiveVHDL 3.1 on a Windows95 PC
--===================================================================--
-----------------------------------------------------------------------
-- Entity for Target device in a PCI bus 33 MHZ 32 bit configuration
-----------------------------------------------------------------------
library ieee,work;
  use ieee.Std_Logic_1164.all;
  use work.Simulation.all;
  use work.PCI_Def.all;	
-----------------------------------------------------------------------
-----------------------------------------------------------------------
entity TG32PCI is
    generic (
      devtype : string(1 to 4); -- type of the device (Fast, Medi, Slow)
      tdelay  : Time;   -- delay time parameter when the device will change
                        -- data on AD_Bus (referenced to CLK signal)
      tsetup  : Time;
      thold   : Time;
      bamem   : Std_Logic_Vector(31 downto 0);  -- base address for memory
      baio    : Std_Logic_Vector(31 downto 0);  -- base address for I/O port
      bacfg   : Std_Logic_Vector(31 downto 0)); -- base address for cfg space
    port (
        -- Address, Data and Command buses (37)
        AD_Bus   : inout Std_Logic_Vector (31 downto 0); -- Address and Data Bus
        C_BE_Bus : in    Std_Logic_Vector (3 downto 0);  -- Command Bus
        PAR      : inout Std_Logic;                      --
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
end TG32PCI; --=================== End of entity ====================--
-----------------------------------------------------------------------
-- Architecture for Target device PCI bus 33MHZ 32 bit configuration
-----------------------------------------------------------------------
architecture Behavior of Target32PCI is
  ---------------------------------------------------------------------
  -- Definition of Memory type,
  ---------------------------------------------------------------------
  type MEMORY is array(0 to 255) of Std_Logic_Vector(31 downto 0);
  ---------------------------------------------------------------------
  -- Local declarations
  ---------------------------------------------------------------------
  shared variable addr      : Std_Logic_Vector (31 downto 0);  -- Address
  shared variable busaddr   : Integer;     -- address present on bus
  shared variable cfgaddr   : Integer;     -- current configuration register address
  shared variable memaddr   : Integer;     -- current memory address
  shared variable ioaddr    : Integer;     -- current I/O port address
  shared variable IOmem     : Memory;      -- IOport registers
  shared variable Cfgmem    : Memory;      -- Configuration registers
  shared variable Mem       : Memory;      -- memory locations
  shared variable trdywaits : Boolean := false; -- wait enable
  shared variable trdy_st,trdy_nr,trdy_loop : Integer := 0;
  ---------------------------------------------------------------------
  -- Signals
  ---------------------------------------------------------------------  
  signal cmd         : Std_Logic_Vector (3 downto 0);   -- Command bus
  signal Busy        : Std_Logic := '0';
  signal IORead      : Std_Logic := '0';
  signal IOWrite     : Std_Logic := '0';
  signal MemRead     : Std_Logic := '0';
  signal MemWrite    : Std_Logic := '0';
  signal WaitWrite   : Std_Logic := '0';
  signal CfgRead     : Std_Logic := '0';
  signal CfgWrite    : Std_Logic := '0';
  signal FrameEv     : Std_Logic := '0';
  signal CmdBusReady : Std_Logic := '0';
  signal TrnArnd     : Std_Logic := '0';
  signal DevAddr     : Std_Logic := '0';
  signal ResFin      : Std_Logic := '0';
  signal Waits       : Std_Logic := '0';
  signal Init        : Std_Logic := '0';
begin--======================== Architecture ========================--
  ---------------------------------------------------------------------
  -- Initialize the memory contents with zeroes
  ---------------------------------------------------------------------
  Initialize : process
  begin
     for i in 0 to 255 loop
         IOmem(i)  := x"00000000";
         Mem(i)    := x"00000000";
         Cfgmem(i) := x"00000000";
     end loop;
     wait;
  end process;
  ---------------------------------------------------------------------
  -- Implements the parity generation and parity checking over the
  -- AD bus and C/BE bus.
  -- Also, generates the PERR_N signal, if the computed parity is not 
  -- equal with PAR signal, when PAR signal is generated by master
  ---------------------------------------------------------------------
  Parity : process(CLK,RST_N)
     variable parbit  : Std_Logic;
     variable lastpar : Std_Logic;
     variable errbit  : Std_Logic;
     variable pargen  : Boolean := false;
     variable errgen  : Boolean := false;
     variable cmdbus  : Std_Logic_Vector(3 downto 0);
     variable addrbus : Std_Logic_Vector(31 downto 0);
  begin
     if (Falling_Edge(RST_N) or RST_N = '0') then
        PAR <= 'Z';
        PERR_N <= 'Z';
     elsif (CLK'Event and CLK = '1') then  -- parity computation on every cycle
         addrbus := AD_Bus;
         cmdbus  := C_BE_Bus;
         lastpar := parbit;
         parbit  := '0';
         if addrbus /= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" then
            for I in 0 to 31 loop
                parbit := parbit xor addrbus(i);
            end loop;
            for I in 0 to 3 loop
                parbit := parbit xor cmdbus(I);
            end loop;
         else
            parbit := 'Z';
         end if;
         if PAR = lastpar then -- PERR computation on every cycle
            errbit := '1';
         elsif PAR /= lastpar then
            errbit := '0';
         elsif PAR = 'Z' then
            errbit := 'H';
         end if;
         if ((IORead = '1' or MemRead = '1' or CfgRead = '1') and DevAddr = '1') then
            pargen := true;
         else
            pargen := false;
         end if;
     elsif (CLK'Event and CLK = '0' and DevAddr = '1') then -- parity generation if necessary
         if errgen = true then
            PERR_N <= errbit;
         else
            PERR_N <= 'H';
         end if;
         if pargen = true then
            PAR <= parbit;
            errgen := false;
         else
            PAR <= 'Z';
            errgen := true;
         end if;
     elsif (CLK'Event and CLK = '0' and DevAddr = '0') then --not the selected device
         PAR    <= 'Z';                            -- by the address
         PERR_N <= 'H';
         SERR_N <= 'Z';
     end if;
  end process;
  ---------------------------------------------------------------------
  -- Implements the command decoding, to receive commands from master
  ---------------------------------------------------------------------
  Decode : process(CLK,FRAME_N,Busy,DevAddr,cmdBusReady,RST_N)
     variable counter : Integer;
     variable devdel  : Boolean := false;
  begin
    if (Falling_Edge(RST_N) or RST_N = '0') then
        DEVSEL_N <= 'Z';
        STOP_N   <= 'Z';
        Busy <= '0';
    elsif (Frame_N'Event and Frame_N = '0') then -- the target device is awakened by 
        FrameEv <= '1';                        -- falling_edge of FRAME signal
        counter := 0;
    elsif (Busy'Event and Busy = '0') then
        IOWrite  <= '0';
        MemWrite <= '0';
        CfgWrite <= '0';
        WaitWrite <= '0';
        IORead   <= '0';
        MemRead  <= '0';
        CfgRead  <= '0';
    elsif (Busy'Event and Busy = '1') then
       if ( IOWrite = '1' or MemWrite = '1' or CfgWrite = '1' or WaitWrite = '1') then
          report "Target device is selected for write operations!"
          severity Note;
          if devtype = "Fast" then
             DEVSEL_N <= '0' after 8 ns;
             Stop_N   <= '1' after 10 ns;
             devdel := false;
          else
             devdel := true;
             counter := 0;
          end if;
       elsif ( IORead = '1' or MemRead = '1' or CfgRead = '1') then
          report "Target device is selected for read operations!"
          severity Note;
          if devtype = "Fast" then
             DEVSEL_N <= '0' after 8 ns;
             Stop_N   <= '1' after 10 ns;
             devdel := false;
          else
             devdel := true;
             counter := 0;
          end if;
       end if;
    elsif (DevAddr'Event and DevAddr = '0') then
       Busy <= '0';
    elsif (cmdBusReady'Event and cmdBusReady = '1') then
       TrnArnd <= '0';
    elsif (CLK'Event and CLK = '0') then
       if Busy = '0' and DevAddr = '1' then  -- deselect
          DEVSEL_N <= 'H';
          STOP_N   <= '1'; 
       elsif DevAddr = '0' then  -- deselect device
          DEVSEL_N <= 'Z';
          STOP_N   <= 'Z';
       end if;
    elsif (CLK'Event and CLK = '1') then
       if ResFin = '1' then    -- memory reserved mode command
          if MemWrite = '1' then  -- master writes to target memory
             if TRDY_N = '0' and IRDY_N = '0' then
                Busy   <= '0';
                ResFin <= '0';
             end if;
          elsif MemRead = '1' then  -- master reads from target memory
             if TrnArnd = '0' and TRDY_N = '0' and IRDY_N = '0' then
                Busy   <= '0';
                ResFin <= '0';
             end if;
          end if;
       end if;
       if devdel = true then
          if devtype = "Medi" then
             if counter = 0 then
                DEVSEL_N <= '0' after 8 ns;
                Stop_N   <= '1' after 10 ns;
                devdel := false;
             end if;
          elsif devtype = "Slow" then
             if counter = 1 then
                DEVSEL_N <= '0' after 8 ns;
                Stop_N   <= '1' after 10 ns;
                devdel := false;
             else
                counter := counter + 1;
             end if;
          end if;
       end if;
       if FRAME_N = '1' then    -- end of cycle
          assert (IRDY_N = '0')
            report "Target device : FRAME signal deassertion error. IRDY is not asserted."
            severity Error;
          if TRDY_N = '0' and IRDY_N = '0' then  -- finish the current cycle
             Busy <= '0';
          end if;
       elsif FrameEv = '1' then -- decoding
          FrameEv <= '0';
          assert (FRAME_N'Last_Event >= tsetup)
            report "Target device : Frame setup time violation in decode cycle!"
            severity warning;
          assert (AD_Bus'Last_Event >= tsetup)
            report "Target device : Address setup time violation in decode cycle!"
            severity warning;
          assert (C_BE_Bus'Last_Event >= tsetup)
            report "Target device : Command setup time violation in decode cycle!"
            severity warning;
          addr := AD_Bus;
          case C_BE_Bus is           -- decoding the command bus
               when "0001" => -- Special Cycle! Used to transfer from master device the 
                              -- wait states parameters
                   if (addr(31 downto 8) = bacfg(31 downto 8) and IDSEL ='1') then
                      WaitWrite <= '1';
                      DevAddr  <= '1';
                      Busy <= '1';
                      trdywaits := true;
                   else        -- this device is not the addressed one,
                      DevAddr  <= '0';   -- so it is not responding
                      trdywaits := false;
                   end if;
                   cfgaddr := Byte2Int(addr(7 downto 0));
               when "0010" => -- I/O Read! Master reads from target device.
                   if addr(31 downto 8) = baio(31 downto 8) then
                       DevAddr <= '1';
                       Busy <= '1';
                       IORead <= '1';
                       TrnArnd <= '1';
                       ioaddr := Byte2Int(addr(7 downto 0));
                   else          -- this device is not the addressed one,
                       DevAddr  <= '0';   -- so it is not responding
                   end if;
               when "0011" => -- I/O Write! Master writes to target device.
                   if addr(31 downto 8) = baio(31 downto 8) then
                       DevAddr <= '1';
                       Busy <= '1';
                       IOWrite <= '1';
                       ioaddr := Byte2Int(addr(7 downto 0));
                   else        -- this device is not the addressed one,
                       DevAddr  <= '0';   -- so it is not responding
                   end if;
               when "1100" => -- Memory Read! Master reads from target device.
                   if addr(31 downto 8) = bamem(31 downto 8) then
                       DevAddr <= '1';
                       Busy <= '1';
                       MemRead <= '1';
                       TrnArnd <= '1';
                       if addr(1 downto 0) = "01" then    -- reserved mode                  
                          ResFin <= '1';
                       elsif addr(1 downto 0) = "11" then -- reserved mode
                          ResFin <= '1';
                       end if;
                       memaddr := Byte2Int(addr(7 downto 0));
                   else     -- this device is not the addressed one,
                       DevAddr  <= '0';   -- so it is not responding
                   end if;
               when "1110" => -- Memory Read Line! Master reads from target device.
                   if addr(31 downto 8) = bamem(31 downto 8) then
                       DevAddr <= '1';
                       Busy <= '1';
                       MemRead <= '1';
                       TrnArnd <= '1';
                       if addr(1 downto 0) = "01" then    -- reserved mode                  
                          ResFin <= '1';
                       elsif addr(1 downto 0) = "11" then -- reserved mode
                          ResFin <= '1';
                       end if;
                       memaddr := Byte2Int(addr(7 downto 0));
                   else     -- this device is not the addressed one,
                       DevAddr  <= '0';   -- so it is not responding
                   end if;
               when "1111" => -- Memory Write! Master writes to target device.
                   if addr(31 downto 8) = bamem(31 downto 8) then
                       DevAddr <= '1';
                       Busy <= '1';
                       MemWrite <= '1';
                       if addr(1 downto 0) = "01" then    -- reserved mode                  
                          ResFin <= '1';
                       elsif addr(1 downto 0) = "11" then -- reserved mode
                          ResFin <= '1';
                       end if;
                       memaddr := Byte2Int(addr(7 downto 0));
                   else      -- this device is not the addressed one,
                       DevAddr  <= '0';   -- so it is not responding
                   end if;
               when "0110" => -- Memory Read! Master reads from target device.
                   if addr(31 downto 8) = bamem(31 downto 8) then
                       DevAddr <= '1';
                       Busy <= '1';
                       MemRead <= '1';
                       TrnArnd <= '1';
                       if addr(1 downto 0) = "01" then    -- reserved mode                  
                          ResFin <= '1';
                       elsif addr(1 downto 0) = "11" then -- reserved mode
                          ResFin <= '1';
                       end if;
                       memaddr := Byte2Int(addr(7 downto 0));
                   else     -- this device is not the addressed one,
                       DevAddr  <= '0';   -- so it is not responding
                   end if;
               when "0111" => -- Memory Write! Master writes to target device.
                   if addr(31 downto 8) = bamem(31 downto 8) then
                       DevAddr <= '1';
                       Busy <= '1';
                       MemWrite <= '1';
                       if addr(1 downto 0) = "01" then    -- reserved mode                  
                          ResFin <= '1';
                       elsif addr(1 downto 0) = "11" then -- reserved mode
                          ResFin <= '1';
                       end if;
                       memaddr := Byte2Int(addr(7 downto 0));
                   else      -- this device is not the addressed one,
                       DevAddr  <= '0';   -- so it is not responding
                   end if;
               when "1010" => -- Configuration Read! Master reads from target device.
                   if (addr(31 downto 8) = bacfg(31 downto 8) and IDSEL = '1') then
                      if addr(1 downto 0) = "01" then
                         report "Target device: Type 1 configuration access on bus! Ignored."
                         severity Note;
                      elsif addr(1 downto 0) = "00" then
                         CfgRead <= '1';
                         TrnArnd <= '1';
                         DevAddr <= '1';
                         Busy <= '1';
                      end if;
                      cfgaddr := Byte2Int(addr(7 downto 0));
                   else       -- this device is not the addressed one,
                      DevAddr  <= '0';   -- so it is not responding
                   end if;
               when "1011" => -- Configuration Write! Master writes to target device.
                   if (addr(31 downto 8) = bacfg(31 downto 8) and IDSEL = '1') then
                      if addr(1 downto 0) = "01" then
                         report "Target device: Type 1 configuration access on bus! Ignored."
                         severity Note;
                      elsif addr(1 downto 0) = "00" then
                         CfgWrite <= '1';
                         DevAddr <= '1';
                         Busy <= '1';
                      end if;
                      cfgaddr := Byte2Int(addr(7 downto 0));
                   else
                             -- this device is not the addressed one,
                      DevAddr  <= '0';   -- so it is not responding
                   end if;
               when "0100" =>
                  report "Target device: Reserved Command detected on C/BE bus! Target will not respond."
                  severity Warning;
               when "0101" =>
                  report "Target device: Reserved Command detected on C/BE bus! Target will not respond."
                  severity Warning;
               when "1000" =>
                  report "Target device: Reserved Command detected on C/BE bus! Target will not respond."
                  severity Warning;
               when "1001" =>
                  report "Target device: Reserved Command detected on C/BE bus! Target will not respond."
                  severity Warning;
               when "ZZZZ" => null;
               when others =>
                  report "Target device: Unknown or invalid command on C/BE bus! Ignored."
                  severity Error;
             end case;
       end if;
    -- elsif (Frame_N'Event and Frame_N = '1') then
    end if;
  end process;
  ---------------------------------------------------------------------
  -- Implementation of Write command. 
  -- Master writes to Target device.
  --------------------------------------------------------------------- 
  WriteProc : process(CLK,RST_N)
      variable waitreg : Std_Logic_Vector(15 downto 0) := x"0000";
      variable Char3_2,Char1,Char0 : Std_Logic_Vector(7 downto 0) := x"00";
  begin
     if (CLK'Event and CLK = '1' and ((IRDY_N = '0' and TRDY_N = '0') or FRAME_N = '1')) then
        assert (AD_Bus'Last_Event >= tsetup)
          report "Target device : Data setup time violation in decode cycle!"
          severity warning;
        assert (C_BE_Bus'Last_Event >= tsetup)
          report "Target device : Byte Enables setup time violation in decode cycle!"
          severity warning;
        if (WaitWrite = '1') then
           Char3_2 := AD_Bus(15 downto 8);
           Char1   := "0000" & AD_Bus(7 downto 4);
           Char0   := "0000" & AD_Bus(3 downto 0);
           trdy_loop := Byte2Int(Char3_2) ;	
           trdy_nr   := Byte2Int(Char1); -- + 1;
           trdy_st   := Byte2Int(Char0);
        elsif IOWrite = '1' then
           -- Master writes to target I/O space
           case addr(1 downto 0) is
               when "00"   =>
                  if C_BE_Bus = "0000" then 
                      IOmem(ioaddr) := AD_Bus;
                  elsif C_BE_Bus = "1000" then
                      IOmem(ioaddr)(23 downto 0) := AD_Bus(23 downto 0);
                  elsif C_BE_Bus = "0100" then
                      IOmem(ioaddr)(31 downto 24) := AD_Bus(31 downto 24);
                      IOmem(ioaddr)(15 downto 0) := AD_Bus(15 downto 0);
                  elsif C_BE_Bus = "1100" then 
                      IOmem(ioaddr)(15 downto 0) := AD_Bus(15 downto 0);
                  elsif C_BE_Bus = "0010" then
                      IOmem(ioaddr)(31 downto 16) := AD_Bus(31 downto 16);
                      IOmem(ioaddr)(7 downto 0) := AD_Bus(7 downto 0);
                  elsif C_BE_Bus = "1010" then
                      IOmem(ioaddr)(23 downto 16) := AD_Bus(23 downto 16);
                      IOmem(ioaddr)(7 downto 0) := AD_Bus(7 downto 0);
                  elsif C_BE_Bus = "0110" then
                      IOmem(ioaddr)(31 downto 24) := AD_Bus(31 downto 24);
                      IOmem(ioaddr)(7 downto 0) := AD_Bus(7 downto 0);
                  elsif C_BE_Bus = "1110" then
                      IOmem(ioaddr)(7 downto 0) := AD_Bus(7 downto 0);
                  elsif C_BE_Bus(0) = '1' then
                      report "Target device: Byte Enable word not valid !"
                      severity Error;
                  end if;
               when "01"   =>
                  if C_BE_Bus = "0001" then 
                      IOmem(ioaddr)(31 downto 8) := AD_Bus(31 downto 8);
                  elsif C_BE_Bus = "1001" then
                      IOmem(ioaddr)(23 downto 8) := AD_Bus(23 downto 8);
                  elsif C_BE_Bus = "0101" then
                      IOmem(ioaddr)(31 downto 24) := AD_Bus(31 downto 24);
                      IOmem(ioaddr)(15 downto 8) := AD_Bus(15 downto 8);
                  elsif C_BE_Bus = "1101" then 
                      IOmem(ioaddr)(15 downto 8) := AD_Bus(15 downto 8);
                  elsif C_BE_Bus(1) = '1' then
                      report "Target device: Byte Enable word not valid !"
                      severity Error;
                  end if;
               when "10"   =>
                  if C_BE_Bus = "0011" then
                      IOmem(ioaddr)(31 downto 16) := AD_Bus(31 downto 16);
                  elsif C_BE_Bus = "1011" then
                      IOmem(ioaddr)(23 downto 16) := AD_Bus(23 downto 16);
                  elsif C_BE_Bus(2) = '1' then
                      report "Target device: Byte Enable word not valid !"
                      severity Error;
                  end if;
               when "11"   =>
                  if C_BE_Bus = "0111" then
                      IOmem(ioaddr)(31 downto 24) := AD_Bus(31 downto 24);
                  elsif C_BE_Bus(3) = '1' then
                      report "Target device: Byte Enable word not valid !"
                      severity Error;
                  end if;
               when others =>
                  null;
           end case;
           ioaddr := ioaddr + 1;
        elsif MemWrite = '1' then
        -- Master writes to target memory space
           case addr(1 downto 0) is
                when "00"   =>  -- linear incrementing mode
                    if C_BE_Bus = "0000" then
                       Mem(memaddr) := AD_Bus;
                    elsif C_BE_Bus = "0001" then
                       Mem(memaddr)(31 downto 8) := AD_Bus(31 downto 8);
                    elsif C_BE_Bus = "0010" then
                       Mem(memaddr)(31 downto 16) := AD_Bus(31 downto 16);
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "0011" then
                       Mem(memaddr)(31 downto 16) := AD_Bus(31 downto 16);
                    elsif C_BE_Bus = "0100" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                       Mem(memaddr)(15 downto 0) := AD_Bus(15 downto 0);
                    elsif C_BE_Bus = "0101" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                       Mem(memaddr)(15 downto 8) := AD_Bus(15 downto 8);
                    elsif C_BE_Bus = "0110" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "0111" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                    elsif C_BE_Bus = "1000" then
                       Mem(memaddr)(23 downto 0) := AD_Bus(23 downto 0);
                    elsif C_BE_Bus = "1001" then
                       Mem(memaddr)(23 downto 8) := AD_Bus(23 downto 8);
                    elsif C_BE_Bus = "1010" then
                       Mem(memaddr)(23 downto 16) := AD_Bus(23 downto 16);
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "1011" then
                       Mem(memaddr)(23 downto 16) := AD_Bus(23 downto 16);
                    elsif C_BE_Bus = "1100" then
                       Mem(memaddr)(15 downto 0) := AD_Bus(15 downto 0);
                    elsif C_BE_Bus = "1101" then
                       Mem(memaddr)(15 downto 8) := AD_Bus(15 downto 8);
                    elsif C_BE_Bus = "1110" then
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "1111" then
                       report "Target device: Byte Enable word not valid !"
                       severity Error;
                    end if;
                    memaddr := memaddr + 1;
               when "01"   =>  -- reserved mode (disconnect after first data phase)
                    if C_BE_Bus = "0000" then
                       Mem(memaddr) := AD_Bus;
                    elsif C_BE_Bus = "0001" then
                       Mem(memaddr)(31 downto 8) := AD_Bus(31 downto 8);
                    elsif C_BE_Bus = "0010" then
                       Mem(memaddr)(31 downto 16) := AD_Bus(31 downto 16);
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "0011" then
                       Mem(memaddr)(31 downto 16) := AD_Bus(31 downto 16);
                    elsif C_BE_Bus = "0100" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                       Mem(memaddr)(15 downto 0) := AD_Bus(15 downto 0);
                    elsif C_BE_Bus = "0101" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                       Mem(memaddr)(15 downto 8) := AD_Bus(15 downto 8);
                    elsif C_BE_Bus = "0110" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "0111" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                    elsif C_BE_Bus = "1000" then
                       Mem(memaddr)(23 downto 0) := AD_Bus(23 downto 0);
                    elsif C_BE_Bus = "1001" then
                       Mem(memaddr)(23 downto 8) := AD_Bus(23 downto 8);
                    elsif C_BE_Bus = "1010" then
                       Mem(memaddr)(23 downto 16) := AD_Bus(23 downto 16);
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "1011" then
                       Mem(memaddr)(23 downto 16) := AD_Bus(23 downto 16);
                    elsif C_BE_Bus = "1100" then
                       Mem(memaddr)(15 downto 0) := AD_Bus(15 downto 0);
                    elsif C_BE_Bus = "1101" then
                       Mem(memaddr)(15 downto 8) := AD_Bus(15 downto 8);
                    elsif C_BE_Bus = "1110" then
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "1111" then
                       report "Target device: Byte Enable word not valid !"
                       severity Error;
                    end if;
               when "10"   =>  -- cacheline wrap mode
                    if C_BE_Bus = "0000" then
                       Mem(memaddr) := AD_Bus;
                    elsif C_BE_Bus = "0001" then
                       Mem(memaddr)(31 downto 8) := AD_Bus(31 downto 8);
                    elsif C_BE_Bus = "0010" then
                       Mem(memaddr)(31 downto 16) := AD_Bus(31 downto 16);
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "0011" then
                       Mem(memaddr)(31 downto 16) := AD_Bus(31 downto 16);
                    elsif C_BE_Bus = "0100" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                       Mem(memaddr)(15 downto 0) := AD_Bus(15 downto 0);
                    elsif C_BE_Bus = "0101" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                       Mem(memaddr)(15 downto 8) := AD_Bus(15 downto 8);
                    elsif C_BE_Bus = "0110" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "0111" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                    elsif C_BE_Bus = "1000" then
                       Mem(memaddr)(23 downto 0) := AD_Bus(23 downto 0);
                    elsif C_BE_Bus = "1001" then
                       Mem(memaddr)(23 downto 8) := AD_Bus(23 downto 8);
                    elsif C_BE_Bus = "1010" then
                       Mem(memaddr)(23 downto 16) := AD_Bus(23 downto 16);
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "1011" then
                       Mem(memaddr)(23 downto 16) := AD_Bus(23 downto 16);
                    elsif C_BE_Bus = "1100" then
                       Mem(memaddr)(15 downto 0) := AD_Bus(15 downto 0);
                    elsif C_BE_Bus = "1101" then
                       Mem(memaddr)(15 downto 8) := AD_Bus(15 downto 8);
                    elsif C_BE_Bus = "1110" then
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "1111" then
                       report "Target device: Byte Enable word not valid !"
                       severity Error;
                    end if;
                    memaddr := memaddr + 1;
               when "11"   =>  -- reserved mode (disconnect after first data phase)
                    if C_BE_Bus = "0000" then
                       Mem(memaddr) := AD_Bus;
                    elsif C_BE_Bus = "0001" then
                       Mem(memaddr)(31 downto 8) := AD_Bus(31 downto 8);
                    elsif C_BE_Bus = "0010" then
                       Mem(memaddr)(31 downto 16) := AD_Bus(31 downto 16);
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "0011" then
                       Mem(memaddr)(31 downto 16) := AD_Bus(31 downto 16);
                    elsif C_BE_Bus = "0100" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                       Mem(memaddr)(15 downto 0) := AD_Bus(15 downto 0);
                    elsif C_BE_Bus = "0101" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                       Mem(memaddr)(15 downto 8) := AD_Bus(15 downto 8);
                    elsif C_BE_Bus = "0110" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "0111" then
                       Mem(memaddr)(31 downto 24) := AD_Bus(31 downto 24);
                    elsif C_BE_Bus = "1000" then
                       Mem(memaddr)(23 downto 0) := AD_Bus(23 downto 0);
                    elsif C_BE_Bus = "1001" then
                       Mem(memaddr)(23 downto 8) := AD_Bus(23 downto 8);
                    elsif C_BE_Bus = "1010" then
                       Mem(memaddr)(23 downto 16) := AD_Bus(23 downto 16);
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "1011" then
                       Mem(memaddr)(23 downto 16) := AD_Bus(23 downto 16);
                    elsif C_BE_Bus = "1100" then
                       Mem(memaddr)(15 downto 0) := AD_Bus(15 downto 0);
                    elsif C_BE_Bus = "1101" then
                       Mem(memaddr)(15 downto 8) := AD_Bus(15 downto 8);
                    elsif C_BE_Bus = "1110" then
                       Mem(memaddr)(7 downto 0) := AD_Bus(7 downto 0);
                    elsif C_BE_Bus = "1111" then
                       report "Target device: Byte Enable word not valid !"
                       severity Error;
                    end if;
               when others =>
                    null;
           end case;
        elsif CfgWrite = '1' then
        -- Master writes to target configuration space
           if C_BE_Bus = "0000" then
              Cfgmem(cfgaddr) := AD_Bus;
           elsif C_BE_Bus = "0001" then
              Cfgmem(cfgaddr)(31 downto 8) := AD_Bus(31 downto 8);
           elsif C_BE_Bus = "0010" then
              Cfgmem(cfgaddr)(31 downto 16) := AD_Bus(31 downto 16);
              Cfgmem(cfgaddr)(7 downto 0) := AD_Bus(7 downto 0);
           elsif C_BE_Bus = "0011" then
              Cfgmem(cfgaddr)(31 downto 16) := AD_Bus(31 downto 16);
           elsif C_BE_Bus = "0100" then
              Cfgmem(cfgaddr)(31 downto 24) := AD_Bus(31 downto 24);
              Cfgmem(cfgaddr)(15 downto 0) := AD_Bus(15 downto 0);
           elsif C_BE_Bus = "0101" then
              Cfgmem(cfgaddr)(31 downto 24) := AD_Bus(31 downto 24);
              Cfgmem(cfgaddr)(15 downto 8) := AD_Bus(15 downto 8);
           elsif C_BE_Bus = "0110" then
              Cfgmem(cfgaddr)(31 downto 24) := AD_Bus(31 downto 24);
              Cfgmem(cfgaddr)(7 downto 0) := AD_Bus(7 downto 0);
           elsif C_BE_Bus = "0111" then
              Cfgmem(cfgaddr)(31 downto 24) := AD_Bus(31 downto 24);
           elsif C_BE_Bus = "1000" then
              Cfgmem(cfgaddr)(23 downto 0) := AD_Bus(23 downto 0);
           elsif C_BE_Bus = "1001" then
              Cfgmem(cfgaddr)(23 downto 8) := AD_Bus(23 downto 8);
           elsif C_BE_Bus = "1010" then
              Cfgmem(cfgaddr)(23 downto 16) := AD_Bus(23 downto 16);
              Cfgmem(cfgaddr)(7 downto 0) := AD_Bus(7 downto 0);
           elsif C_BE_Bus = "1011" then
              Cfgmem(cfgaddr)(23 downto 16) := AD_Bus(23 downto 16);
           elsif C_BE_Bus = "1100" then
              Cfgmem(cfgaddr)(15 downto 0) := AD_Bus(15 downto 0);
           elsif C_BE_Bus = "1101" then
              Cfgmem(cfgaddr)(15 downto 8) := AD_Bus(15 downto 8);
           elsif C_BE_Bus = "1110" then
              Cfgmem(cfgaddr)(7 downto 0) := AD_Bus(7 downto 0);
           elsif C_BE_Bus = "1111" then
              report "Target device: Byte Enable word not valid !"
              severity Error;
           end if;
           cfgaddr := cfgaddr + 1;
        end if;
    end if;
  end process;
  ---------------------------------------------------------------------
  -- Implementation of Read command. 
  -- Master read from Target device.
  --------------------------------------------------------------------- 
  ReadProc : process(RST_N,CLK,cmdBusReady,IORead,IOWrite,MemRead,MemWrite,CfgRead,CfgWrite)
      variable first : Boolean := true;
  begin
     if (Falling_Edge(RST_N) or RST_N = '0') then
        AD_Bus <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";        
     elsif (CLK'Event and CLK = '0') then
        cmdBusReady <= '0';
        if (first = true or TrnArnd ='1') then
     -- Initialize the AD_Bus to avoid bus conflict
           AD_Bus <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ";
           first := false;
        elsif Init = '1' then
           Init <= '0';
           AD_Bus <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" after tdelay;
        end if;
     elsif IORead'Event and IORead = '0' then
           Init <= '1';
     elsif MemRead'Event and MemRead = '0' then
           Init <= '1';
     elsif CfgRead'Event and CfgRead = '0' then
           Init <= '1';
     elsif (CLK'Event and CLK = '1' and IRDY_N = '0') then
        if (IORead = '1' or MemRead = '1' or CfgRead = '1') then
           cmd <= C_BE_Bus; -- read the byte enable command
           cmdBusReady <= '1';
        end if;
     elsif (cmdBusReady'Event and cmdBusReady = '0' and TRDY_N = '0') then
         if IORead = '1' then
     -- Master reads from target I/O space
            case addr(1 downto 0) is
               when "00"   =>
                  if cmd = "0000" then 
                      AD_Bus <= IOmem(ioaddr) after tdelay;
                  elsif cmd = "1000" then
                      AD_Bus(31 downto 24) <= "11111111" after tdelay;
                      AD_Bus(23 downto 0)  <= IOmem(ioaddr)(23 downto 0) after tdelay;
                  elsif cmd = "0100" then
                      AD_Bus(31 downto 24) <= IOmem(ioaddr)(31 downto 24) after tdelay;
                      AD_Bus(23 downto 16) <= "11111111" after tdelay;
                      AD_Bus(15 downto 0)  <= IOmem(ioaddr)(15 downto 0) after tdelay;
                  elsif cmd = "1100" then 
                      AD_Bus(31 downto 16) <= "1111111111111111" after tdelay;
                      AD_Bus(15 downto 0)  <= IOmem(ioaddr)(15 downto 0) after tdelay;
                  elsif cmd = "0010" then
                      AD_Bus(31 downto 16) <= IOmem(ioaddr)(31 downto 16) after tdelay;
                      AD_Bus(15 downto 8)  <= "11111111" after tdelay;
                      AD_Bus(7 downto 0)   <= IOmem(ioaddr)(7 downto 0) after tdelay;
                  elsif cmd = "1010" then
                      AD_Bus(31 downto 24) <= "11111111" after tdelay;
                      AD_Bus(23 downto 16) <= IOmem(ioaddr)(23 downto 16) after tdelay;
                      AD_Bus(15 downto 8)  <= "11111111" after tdelay;
                      AD_Bus(7 downto 0)   <= IOmem(ioaddr)(7 downto 0) after tdelay;
                  elsif cmd = "0110" then
                      AD_Bus(31 downto 24) <= IOmem(ioaddr)(31 downto 24) after tdelay;
                      AD_Bus(23 downto 8)  <= "1111111111111111" after tdelay;
                      AD_Bus(7 downto 0)   <= IOmem(ioaddr)(7 downto 0) after tdelay;
                  elsif cmd = "1110" then
                      AD_Bus(31 downto 8) <= "111111111111111111111111" after tdelay;
                      AD_Bus(7 downto 0)  <= IOmem(ioaddr)(7 downto 0) after tdelay;
                  elsif cmd(0) = '1' then
                      report "Target device: Byte Enable word not valid !"
                      severity Error;
                  end if;
               when "01"   =>
                  if cmd = "0001" then 
                      AD_Bus(31 downto 8) <= IOmem(ioaddr)(31 downto 8) after tdelay;
                      AD_Bus(7 downto 0)  <= "11111111" after tdelay;
                  elsif cmd = "1001" then
                      AD_Bus(31 downto 24) <= "11111111" after tdelay;
                      AD_Bus(23 downto 8)  <= IOmem(ioaddr)(23 downto 8) after tdelay;
                      AD_Bus(7 downto 0)   <= "11111111" after tdelay;
                  elsif cmd = "0101" then
                      AD_Bus(31 downto 24) <= IOmem(ioaddr)(31 downto 24) after tdelay;
                      AD_Bus(23 downto 16) <= "11111111" after tdelay;
                      AD_Bus(15 downto 8)  <= IOmem(ioaddr)(15 downto 8) after tdelay;
                      AD_Bus(7 downto 0)   <= "11111111" after tdelay;
                  elsif cmd = "1101" then
                      AD_Bus(31 downto 16) <= "1111111111111111" after tdelay;
                      AD_Bus(15 downto 8)  <= IOmem(ioaddr)(15 downto 8) after tdelay;
                      AD_Bus(7 downto 0)   <= "11111111" after tdelay;
                  elsif cmd(1) = '1' then
                      report "Target device: Byte Enable word not valid !"
                      severity Error;
                  end if;
               when "10"   =>
                  if cmd = "0011" then
                      AD_Bus(31 downto 16) <= IOmem(ioaddr)(31 downto 16) after tdelay;
                      AD_Bus(15 downto 0)  <= "1111111111111111" after tdelay;
                  elsif cmd = "1011" then
                      AD_Bus(31 downto 24) <= "11111111" after tdelay;
                      AD_Bus(23 downto 16) <= IOmem(ioaddr)(23 downto 16) after tdelay;
                      AD_Bus(15 downto 0)  <= "1111111111111111" after tdelay;
                  elsif cmd(2) = '1' then
                      report "Target device: Byte Enable word not valid !"
                      severity Error;
                  end if;
               when "11"   =>
                  if cmd = "0111" then
                      AD_Bus(31 downto 24) <= IOmem(ioaddr)(31 downto 24) after tdelay;
                      AD_Bus(23 downto 0)  <= "111111111111111111111111" after tdelay;
                  elsif cmd(3) = '1' then
                      report "Target device: Byte Enable word not valid !"
                      severity Error;
                  end if;
               when others =>
                  null;
            end case;
            ioaddr := ioaddr + 1;
         elsif MemRead = '1' then
     -- Master reads from target memory space
            case addr(1 downto 0) is
               when "00"   =>  -- linear incrementing mode
                    if cmd = "0000" then
                       AD_Bus <= Mem(memaddr) after tdelay;
                    elsif cmd = "0001" then
                       AD_Bus(31 downto 8) <= Mem(memaddr)(31 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "0010" then
                       AD_Bus(31 downto 16) <= Mem(memaddr)(31 downto 16) after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                       AD_Bus(15 downto 8) <= "11111111" after tdelay;
                    elsif cmd = "0011" then
                       AD_Bus(31 downto 16) <= Mem(memaddr)(31 downto 16) after tdelay;
                       AD_Bus(15 downto 0) <= "1111111111111111" after tdelay;
                    elsif cmd = "0100" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(15 downto 0) <= Mem(memaddr)(15 downto 0) after tdelay;
                       AD_Bus(23 downto 16) <= "11111111" after tdelay;
                    elsif cmd = "0101" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(23 downto 16) <= "11111111" after tdelay;
                       AD_Bus(15 downto 8) <= Mem(memaddr)(15 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "0110" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(23 downto 8) <= "1111111111111111" after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                    elsif cmd = "0111" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(23 downto 0) <= "111111111111111111111111" after tdelay;
                    elsif cmd = "1000" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 0) <= Mem(memaddr)(23 downto 0) after tdelay;
                    elsif cmd = "1001" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 8) <= Mem(memaddr)(23 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "1010" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 16) <= Mem(memaddr)(23 downto 16) after tdelay;
                       AD_Bus(15 downto 8) <= "11111111" after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                    elsif cmd = "1011" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 16) <= Mem(memaddr)(23 downto 16) after tdelay;
                       AD_Bus(15 downto 0) <= "1111111111111111" after tdelay;
                    elsif cmd = "1100" then
                       AD_Bus(31 downto 16) <= "1111111111111111" after tdelay;
                       AD_Bus(15 downto 0) <= Mem(memaddr)(15 downto 0) after tdelay;
                    elsif cmd = "1101" then
                       AD_Bus(31 downto 16) <= "1111111111111111" after tdelay;
                       AD_Bus(15 downto 8) <= Mem(memaddr)(15 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "1110" then
                       AD_Bus(31 downto 8) <= "111111111111111111111111" after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                    elsif cmd = "1111" then
                       report "Target device: Byte Enable word not valid !"
                       severity Error;
                    end if;
                    memaddr := memaddr + 1;
               when "01"   =>  -- reserved mode (disconnect after first data phase)
                    if cmd = "0000" then
                       AD_Bus <= Mem(memaddr) after tdelay;
                    elsif cmd = "0001" then
                       AD_Bus(31 downto 8) <= Mem(memaddr)(31 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "0010" then
                       AD_Bus(31 downto 16) <= Mem(memaddr)(31 downto 16) after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                       AD_Bus(15 downto 8) <= "11111111" after tdelay;
                    elsif cmd = "0011" then
                       AD_Bus(31 downto 16) <= Mem(memaddr)(31 downto 16) after tdelay;
                       AD_Bus(15 downto 0) <= "1111111111111111" after tdelay;
                    elsif cmd = "0100" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(15 downto 0) <= Mem(memaddr)(15 downto 0) after tdelay;
                       AD_Bus(23 downto 16) <= "11111111" after tdelay;
                    elsif cmd = "0101" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(23 downto 16) <= "11111111" after tdelay;
                       AD_Bus(15 downto 8) <= Mem(memaddr)(15 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "0110" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(23 downto 8) <= "1111111111111111" after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                    elsif cmd = "0111" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(23 downto 0) <= "111111111111111111111111" after tdelay;
                    elsif cmd = "1000" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 0) <= Mem(memaddr)(23 downto 0) after tdelay;
                    elsif cmd = "1001" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 8) <= Mem(memaddr)(23 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "1010" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 16) <= Mem(memaddr)(23 downto 16) after tdelay;
                       AD_Bus(15 downto 8) <= "11111111" after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                    elsif cmd = "1011" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 16) <= Mem(memaddr)(23 downto 16) after tdelay;
                       AD_Bus(15 downto 0) <= "1111111111111111" after tdelay;
                    elsif cmd = "1100" then
                       AD_Bus(31 downto 16) <= "1111111111111111" after tdelay;
                       AD_Bus(15 downto 0) <= Mem(memaddr)(15 downto 0) after tdelay;
                    elsif cmd = "1101" then
                       AD_Bus(31 downto 16) <= "1111111111111111" after tdelay;
                       AD_Bus(15 downto 8) <= Mem(memaddr)(15 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "1110" then
                       AD_Bus(31 downto 8) <= "111111111111111111111111" after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                    elsif cmd = "1111" then
                       report "Target device: Byte Enable word not valid !"
                       severity Error;
                    end if;
               when "10"   =>  -- cacheline wrap mode
                    if cmd = "0000" then
                       AD_Bus <= Mem(memaddr) after tdelay;
                    elsif cmd = "0001" then
                       AD_Bus(31 downto 8) <= Mem(memaddr)(31 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "0010" then
                       AD_Bus(31 downto 16) <= Mem(memaddr)(31 downto 16) after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                       AD_Bus(15 downto 8) <= "11111111" after tdelay;
                    elsif cmd = "0011" then
                       AD_Bus(31 downto 16) <= Mem(memaddr)(31 downto 16) after tdelay;
                       AD_Bus(15 downto 0) <= "1111111111111111" after tdelay;
                    elsif cmd = "0100" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(15 downto 0) <= Mem(memaddr)(15 downto 0) after tdelay;
                       AD_Bus(23 downto 16) <= "11111111" after tdelay;
                    elsif cmd = "0101" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(23 downto 16) <= "11111111" after tdelay;
                       AD_Bus(15 downto 8) <= Mem(memaddr)(15 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "0110" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(23 downto 8) <= "1111111111111111" after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                    elsif cmd = "0111" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(23 downto 0) <= "111111111111111111111111" after tdelay;
                    elsif cmd = "1000" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 0) <= Mem(memaddr)(23 downto 0) after tdelay;
                    elsif cmd = "1001" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 8) <= Mem(memaddr)(23 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "1010" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 16) <= Mem(memaddr)(23 downto 16) after tdelay;
                       AD_Bus(15 downto 8) <= "11111111" after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                    elsif cmd = "1011" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 16) <= Mem(memaddr)(23 downto 16) after tdelay;
                       AD_Bus(15 downto 0) <= "1111111111111111" after tdelay;
                    elsif cmd = "1100" then
                       AD_Bus(31 downto 16) <= "1111111111111111" after tdelay;
                       AD_Bus(15 downto 0) <= Mem(memaddr)(15 downto 0) after tdelay;
                    elsif cmd = "1101" then
                       AD_Bus(31 downto 16) <= "1111111111111111" after tdelay;
                       AD_Bus(15 downto 8) <= Mem(memaddr)(15 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "1110" then
                       AD_Bus(31 downto 8) <= "111111111111111111111111" after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                    elsif cmd = "1111" then
                       report "Target device: Byte Enable word not valid !"
                       severity Error;
                    end if;
                    memaddr := memaddr + 1;
               when "11"   =>  -- reserved mode (disconnect after first data phase)
                    if cmd = "0000" then
                       AD_Bus <= Mem(memaddr) after tdelay;
                    elsif cmd = "0001" then
                       AD_Bus(31 downto 8) <= Mem(memaddr)(31 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "0010" then
                       AD_Bus(31 downto 16) <= Mem(memaddr)(31 downto 16) after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                       AD_Bus(15 downto 8) <= "11111111" after tdelay;
                    elsif cmd = "0011" then
                       AD_Bus(31 downto 16) <= Mem(memaddr)(31 downto 16) after tdelay;
                       AD_Bus(15 downto 0) <= "1111111111111111" after tdelay;
                    elsif cmd = "0100" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(15 downto 0) <= Mem(memaddr)(15 downto 0) after tdelay;
                       AD_Bus(23 downto 16) <= "11111111" after tdelay;
                    elsif cmd = "0101" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(23 downto 16) <= "11111111" after tdelay;
                       AD_Bus(15 downto 8) <= Mem(memaddr)(15 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "0110" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(23 downto 8) <= "1111111111111111" after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                    elsif cmd = "0111" then
                       AD_Bus(31 downto 24) <= Mem(memaddr)(31 downto 24) after tdelay;
                       AD_Bus(23 downto 0) <= "111111111111111111111111" after tdelay;
                    elsif cmd = "1000" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 0) <= Mem(memaddr)(23 downto 0) after tdelay;
                    elsif cmd = "1001" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 8) <= Mem(memaddr)(23 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "1010" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 16) <= Mem(memaddr)(23 downto 16) after tdelay;
                       AD_Bus(15 downto 8) <= "11111111" after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                    elsif cmd = "1011" then
                       AD_Bus(31 downto 24) <= "11111111" after tdelay;
                       AD_Bus(23 downto 16) <= Mem(memaddr)(23 downto 16) after tdelay;
                       AD_Bus(15 downto 0) <= "1111111111111111" after tdelay;
                    elsif cmd = "1100" then
                       AD_Bus(31 downto 16) <= "1111111111111111" after tdelay;
                       AD_Bus(15 downto 0) <= Mem(memaddr)(15 downto 0) after tdelay;
                    elsif cmd = "1101" then
                       AD_Bus(31 downto 16) <= "1111111111111111" after tdelay;
                       AD_Bus(15 downto 8) <= Mem(memaddr)(15 downto 8) after tdelay;
                       AD_Bus(7 downto 0) <= "11111111" after tdelay;
                    elsif cmd = "1110" then
                       AD_Bus(31 downto 8) <= "111111111111111111111111" after tdelay;
                       AD_Bus(7 downto 0) <= Mem(memaddr)(7 downto 0) after tdelay;
                    elsif cmd = "1111" then
                       report "Target device: Byte Enable word not valid !"
                       severity Error;
                    end if;
               when others =>
                    null;
            end case;
         elsif CfgRead = '1' then        
     -- Master reads from target configuration space
            if cmd = "0000" then
               AD_Bus <= Cfgmem(cfgaddr) after tdelay;
            elsif cmd = "0001" then
               AD_Bus(31 downto 8) <= Cfgmem(cfgaddr)(31 downto 8) after tdelay;
               AD_Bus(7 downto 0) <= "11111111" after tdelay;
            elsif cmd = "0010" then
               AD_Bus(31 downto 16) <= Cfgmem(cfgaddr)(31 downto 16) after tdelay;
               AD_Bus(15 downto 8) <= "11111111" after tdelay;
               AD_Bus(7 downto 0) <= Cfgmem(cfgaddr)(7 downto 0) after tdelay;
            elsif cmd = "0011" then
               AD_Bus(31 downto 16) <= Cfgmem(cfgaddr)(31 downto 16) after tdelay;
               AD_Bus(15 downto 0) <= "1111111111111111" after tdelay;
            elsif cmd = "0100" then
               AD_Bus(31 downto 24) <= Cfgmem(cfgaddr)(31 downto 24) after tdelay;
               AD_Bus(15 downto 0) <= Cfgmem(cfgaddr)(15 downto 0) after tdelay;
               AD_Bus(23 downto 16) <= "11111111" after tdelay;
            elsif cmd = "0101" then
               AD_Bus(31 downto 24) <= Cfgmem(cfgaddr)(31 downto 24) after tdelay;
               AD_Bus(23 downto 16) <= "11111111" after tdelay;
               AD_Bus(15 downto 8) <= Cfgmem(cfgaddr)(15 downto 8) after tdelay;
               AD_Bus(7 downto 0) <= "11111111" after tdelay;
            elsif cmd = "0110" then
               AD_Bus(31 downto 24) <= Cfgmem(cfgaddr)(31 downto 24) after tdelay;
               AD_Bus(23 downto 8) <= "1111111111111111" after tdelay;
               AD_Bus(7 downto 0) <= Cfgmem(cfgaddr)(7 downto 0) after tdelay;
            elsif cmd = "0111" then
               AD_Bus(31 downto 24) <= Cfgmem(cfgaddr)(31 downto 24) after tdelay;
               AD_Bus(23 downto 0) <= "111111111111111111111111" after tdelay;
            elsif cmd = "1000" then
               AD_Bus(31 downto 24) <= "11111111" after tdelay;
               AD_Bus(23 downto 0) <= Cfgmem(cfgaddr)(23 downto 0) after tdelay;
            elsif cmd = "1001" then
               AD_Bus(31 downto 24) <= "11111111" after tdelay;
               AD_Bus(23 downto 8) <= Cfgmem(cfgaddr)(23 downto 8) after tdelay;
               AD_Bus(7 downto 0) <= "11111111" after tdelay;
            elsif cmd = "1010" then
               AD_Bus(31 downto 24) <= "11111111" after tdelay;
               AD_Bus(23 downto 16) <= Cfgmem(cfgaddr)(23 downto 16) after tdelay;
               AD_Bus(15 downto 8) <= "11111111" after tdelay;
               AD_Bus(7 downto 0) <= Cfgmem(cfgaddr)(7 downto 0) after tdelay;
            elsif cmd = "1011" then
               AD_Bus(31 downto 24) <= "11111111" after tdelay;
               AD_Bus(23 downto 16) <= Cfgmem(cfgaddr)(23 downto 16) after tdelay;
               AD_Bus(15 downto 0) <= "1111111111111111" after tdelay;
            elsif cmd = "1100" then
               AD_Bus(31 downto 16) <= "1111111111111111" after tdelay;
               AD_Bus(15 downto 0) <= Cfgmem(cfgaddr)(15 downto 0) after tdelay;
            elsif cmd = "1101" then
               AD_Bus(31 downto 16) <= "1111111111111111" after tdelay;
               AD_Bus(15 downto 8) <= Cfgmem(cfgaddr)(15 downto 8) after tdelay;
               AD_Bus(7 downto 0) <= "11111111" after tdelay;
            elsif cmd = "1110" then
               AD_Bus(31 downto 8) <= "111111111111111111111111" after tdelay;
               AD_Bus(7 downto 0) <= Cfgmem(cfgaddr)(7 downto 0) after tdelay;
            elsif cmd = "1111" then
               report "Target device: Byte Enable word not valid !"
               severity Error;
            end if;
            cfgaddr := cfgaddr + 1;
         end if;
     end if;
  end process;
  ---------------------------------------------------------------------
  -- Implements the wait states process.
  -- Target generates wait states accordingly to wait states parameters
  -- received from master during Special Cycle.
  -- Master reads the parameters from commands file.
  --------------------------------------------------------------------- 
  WaitProc : process(CLK,FRAME_N,RST_N)
     variable counter : Integer;
     variable waitcnt : Integer;
     variable start   : Boolean;
  begin
     if (Falling_Edge(RST_N) or RST_N = '0') then
        TRDY_N <= 'Z';
     elsif (FRAME_N'Event and FRAME_N = '0') then
        counter := 0;
        start := true;
        TRDY_N <= '1';
     elsif (CLK'Event and CLK = '0') then
        if (Busy = '0' and DevAddr = '0') then  -- deselect device
           TRDY_N <= 'H';
        elsif (Busy = '0' and DevAddr = '1') then  -- deselect signal
           TRDY_N <= '1';
        end if;
        if (DevAddr = '1' and TrnArnd = '0' and DEVSEL_N = '0') then
           if (counter >= trdy_st and start = true and TRDY_N = '1') then
              -- finish waitstates at start
              TRDY_N <= '0';
              start := false;
           elsif (counter = trdy_loop and TRDY_N = '0') then
              -- random waitstates 
              TRDY_N <= '1';
              waits <= '1';
              waitcnt := 1;
           elsif (waitcnt = trdy_nr and waits = '1') then
              -- finish random waitstates
              waits <= '0';
              TRDY_N <= '0';
           elsif waits = '1' then -- count random waitstates
              waitcnt := waitcnt + 1;
           end if;
        end if;
     counter := counter + 1;
     end if;
  end process;
  ---------------------------------------------------------------------
  -- Implements the assertion process for IRDY_N, FRAME_N, and IDSEL 
  -- signals.
  --------------------------------------------------------------------- 
  AsrtProc : process(CLK,FRAME_N,IRDY_N,IDSEL,AD_Bus)
     variable irdevs  : Boolean := false;
     variable idsevs  : Boolean := false;
     variable counter : Integer;
  begin
     if Rising_Edge(CLK) and FRAME_N = '0' and IRDY_N = '1' then
        counter := counter + 1;
        if counter >= 8 then
           report"Target device:IRDY is not asserted within 8 clocks from FRAME assertion!"
           severity warning;
        end if;
     end if;
     if AD_Bus'Event then
         assert(CLK'Last_Event <= thold)
           report"Target device: Address or Data hold time violation!"
           severity warning;
     end if;
     if Rising_Edge(CLK) and DevAddr = '1' then
        if irdevs = true then
           irdevs := false;
           assert(IRDY_N'Last_Event >= tsetup)
             report"Target device: IRDY setup time violation in current data transfer!"
             severity warning;
        end if;
        if idsevs = true then
           idsevs := false;
           assert(IDSEL'Last_Event >= tsetup)
             report"Target device: IDSEL setup time violation!"
             severity warning;
        end if;
        if FRAME_N = '0' and IRDY_N = '1' then
           counter := counter + 1 ;
        end if;
        assert (counter <= 8)
          report"Target device: IRDY has not been asserted within 8 clocks from FRAME assertion!"
          severity warning;
     elsif Falling_Edge(FRAME_N) then
           counter := 0;
     elsif Rising_Edge(FRAME_N) then
         assert(CLK'Last_Event <= thold)
           report"Target device: FRAME hold time violation in current data transfer!"
           severity warning;
     elsif Falling_Edge(IRDY_N) and DevAddr = '1' then
        irdevs := true;
     elsif Rising_Edge(IRDY_N) and DevAddr = '1' then
         assert(CLK'Last_Event <= thold)
           report"Target device: IRDY hold time violation in current data transfer!"
           severity warning;
     elsif Rising_Edge(IDSEL) and DevAddr = '1' then
         idsevs := true;
     elsif Falling_Edge(IDSEL) and DevAddr = '1' then
         assert(CLK'Last_Event <= thold)
           report"Target device: IDSEL hold time violation!"
           severity warning;
     end if;
  end process;
end Behavior; --================ End of architecture ================--
-----------------------------------------------------------------------
-- Revision list
-- Version     Author          Date           Changes
--
-- 0.1       Ovidiu Lupas   June 09, 2000     New model
-----------------------------------------------------------------------

