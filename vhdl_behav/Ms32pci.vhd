--===================================================================--
--
--  www.OpenCores.Org - June 2000
--  This model adheres to the GNU public license  
--
-- Design units : Master device for PCI Local Bus 33 MHz 32 bits
--                  (BoardLevel Simulation model)
--                  (Entity and architecture)
--
-- File name    : MS32PCI.vhd
--
-- Purpose      : The Master device is used to simulate a master 
--                device on the PCI-Bus
--
-- Note         : This model is modelled after the PCI protocol 
--                as described in Xilinx & Altera AppNotes
--
-- Limitations  : None known
--
-- Errors       : None known
--
-- Library      : PCI_Lib.vhd
--
-- Dependencies : IEEE.Std_Logic_1164
--
-- Author       : Ovidiu Lupas
--                olupas@opencores.org
--
-- Simulator    : ModelSim EE version 5.2 on a Windows95 PC
--                ActiveVHDL 3.1 on a Windows95 PC
--===================================================================--
library ieee,work;
   use ieee.Std_Logic_1164.all;
   use work.Simulation.all;
   use std.textio.all;
   use work.PCI_Def.all;
-----------------------------------------------------------------------
--   ENTITY FOR MASTER PCI SIMULATION MODEL                          --
-----------------------------------------------------------------------
entity MS32PCI is
   generic (
      cmd_file : string := "PCI.CMD"; -- the commands file
      tdelay   : Time   := 2 ns;      -- delay time parameter
      tsetup   : Time   := 7 ns;      -- setup time to be checked
      thold    : Time   := 0 ns);     -- hold time to be checked
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
      IDSEL    : in    STD_LOGIC;
      -- Error reporting signals (2)
      PERR_N   : inout STD_LOGIC;
      SERR_N   : inout STD_LOGIC;
      -- Arbitration signals (2)
      REQ_N    : out   STD_LOGIC; 
      GNT_N    : in    STD_LOGIC;
      -- System signals (2)
      CLK      : in    STD_LOGIC;
      RST_N    : in    STD_LOGIC);
end MS32PCI;--================== End of entity ======================--
-----------------------------------------------------------------------
-- Architecture for Master device : PCI bus 33MHZ 32 bit configuration
-----------------------------------------------------------------------
architecture Behavior of MS32PCI is
  ---------------------------------------------------------------------
  -- Signals
  ---------------------------------------------------------------------
  signal parity_now    : Std_Logic; -- internal variable (calculate parity)
  signal parity_read   : Std_Logic; -- internal variable (parity at read)
  signal parity_flag   : Boolean;   -- internal variable (write ON/OFF on line PERR_N)
  signal PAR_READ_TEMP : Std_Logic; -- insert or no signal IRDY_N
  signal PAR_READ_FLAG : Std_Logic; -- insert or no signal IRDY_N
  ---------------------------------------------------------------------
  -- Variables
  ---------------------------------------------------------------------
  shared variable RESET : Integer;
begin --======================= Architecture ========================-- 
  ---------------------------------------------------------------------
  -- Process is used to initialize command 
  ---------------------------------------------------------------------
  RSTproc : process(RST_N)
  begin
    if not RST_N'STABLE and RST_N ='0' then
      RESET := 1;
    end if;
  end process;
  ---------------------------------------------------------------------
  -- Implements the parity generation and parity checking over the
  -- AD bus and C/BE bus.
  -- Also, generates the PERR_N signal, if the computed parity is not 
  -- equal with PAR signal, when PAR signal is generated by target
  ---------------------------------------------------------------------
  ParGen : process(CLK)
      variable PERR_N_TEMP : Std_Logic;
  begin
    if not CLK'STABLE and CLK = '0' then
       PAR <= parity_now after tdelay;      -- PAR ='1','0' or 'Z'
       PERR_N <= PERR_N_TEMP after tdelay ;
       SERR_N <= PERR_N_TEMP ;
       PAR_READ_TEMP <= parity_read ; 

       if parity_flag = true then
          PAR_READ_FLAG <= '1';
       else
          PAR_READ_FLAG <= '0';
       end if;
                  
       if PAR_READ_FLAG = '1' then   -- 
          if (PAR = PAR_READ_TEMP) then  -- MASTER sets PERR_N
             PERR_N <= '1' after tdelay;
          else 
             if PAR_READ_TEMP = 'Z' then
                PERR_N <= 'H' after tdelay;
             else          
                PERR_N <= '0' after tdelay;        
             end if;   
          end if;  
       else
         PERR_N <= 'H' after tdelay;       
       end if;       
    end if;
  end process;
  ---------------------------------------------------------------------
  -- MAIN PROCESS FOR MASTER                                                 --
  ---------------------------------------------------------------------
  MS32PCI_MAIN : process
     variable Data_array     : Data_buffer; --  data array
     variable data_last_read : Boolean;           
     variable irdy_start     : Integer; -- variable is actualize
     variable irdy_loop      : Integer;
     variable irdy_nr        : Integer; -- by command WAITC
     variable irdy_insert    : Boolean; -- assert or not IRDY_N
     ------------------------------------------------------------------
     -- Procedure is used to initialize MASTER and irdy_** variables --
     ------------------------------------------------------------------
     procedure Init is
     begin
        RESET := 0;
        AD_Bus   <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" ; -- Address and Data Bus
        C_BE_Bus <= "ZZZZ";            -- Command Bus
        PAR      <= 'Z';                      
        PERR_N   <= 'Z';
        REQ_N    <= 'H';
        SERR_N   <= 'H';   
         irdy_start := 0;                       -- number of IRDY state
         irdy_nr := 0;
         irdy_loop := 255;
         parity_flag <= false;
        if irdy_loop = 0 or irdy_nr = 0 then 
           irdy_insert := false;
        else
           irdy_insert := true;
        end if;          
     end Init;
     ------------------------------------------------------------------
     -- This procedure calculate parity of signals address_data(31..0) 
     -- and c_be(3..0) and return par_bit                      
     ------------------------------------------------------------------
     procedure PARITY(
        address_data : in    STD_LOGIC_VECTOR(31 downto 0); 
        c_be         : in    STD_LOGIC_VECTOR(3 downto 0);
        par_bit      : inout STD_LOGIC)   is  
     begin
        par_bit := '0';
        for I in 0 to 31 loop
            par_bit := par_bit xor address_data(I);
        end loop;

        for I in 0 to 3 loop
            par_bit := par_bit xor c_be(I);
        end loop;

        if (par_bit = 'X' or par_bit = 'U') then
          par_bit := 'Z';
        end if;
     end PARITY;
     --------------------------------------------------------------------------
     -- This procedure is used for READ_Bus and WRITE_Bus operation          --
     --------------------------------------------------------------------------
     procedure READ_WRITE(
        address : in STD_LOGIC_VECTOR(31 downto 0); -- address
        data    : in Data_buffer;                   -- data to write operation
        data_nr : in Integer;                       -- number of data DWORD(32 bit)
        bus_cmd : in STD_LOGIC_VECTOR(3 downto 0);  -- bus command
        bus_sel : in Data_Enable;                   -- C/BE lines
        rd_wr   : in STD_LOGIC)  is                 -- select read or write operation
          variable data_number : Integer;           -- number of data to read and write
          variable data_read   : Std_Logic_Vector(31 downto 0);   -- data read
          variable data_old    : Std_Logic_Vector(31 downto 0);   -- data read old
          variable stop        : Boolean;   -- internal variable (determined by STOP_N) 
          variable str8        : string(1 to 8);         
          variable Good2       : Boolean;
          variable nr_irdy     : Integer;   -- duration of IRDY pulse
          variable loop_irdy   : Integer;   -- position of IRDY pulse
          variable start_irdy  : Integer;   -- used for master-abord termination
          variable parity_temp : Std_Logic; -- internal variable
          variable trdy_stop   : Integer;   -- internal variable
          variable trdy_exit   : Boolean;   -- internal variable
     begin
        if GNT_N /= '1' then
           wait until FALLING_EDGE(CLK);           -- start cycle
        end if;   

        while (GNT_N /= '0' and GNT_N /= 'L') and (RESET = 0) loop
          wait until FALLING_EDGE(CLK);
           AD_Bus(31 downto 0) <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" after tdelay;
           C_BE_Bus(3 downto 0) <= "ZZZZ" after tdelay;  
           FRAME_N <= 'Z';-- after tdelay;                                  
           IRDY_N <= 'Z' after tdelay;
           parity_now <= 'Z' after tdelay;
           REQ_N <= '0' after tdelay;
        end loop;

        if (RESET = 0) then
        -- exit curent instruction if signal RST_N is active 
        -- GM
        --- acces to the bus has been granted
           data_number := data_nr;       -- number of DWORDs for transfer
           stop := false;                -- 
           start_irdy := 3;              -- 
           nr_irdy := irdy_nr;           -- actualize internal variable
           loop_irdy := irdy_loop;       -- --"--
           irdy_insert := true;          -- --"-- 
           trdy_stop := 8;               -- wait maximum 8 clock cycles for TRDY
           trdy_exit := false;

           if rd_wr = '1' then           -- READ /WRITE CYCLE
           --------------------------------------------------------------------
           -- BEGIN READ CYCLE                                               --
           --------------------------------------------------------------------
             -- address phase
             AD_Bus(31 downto 0) <= address(31 downto 0) after tdelay; -- set address  
             C_BE_Bus(3 downto 0) <= bus_cmd(3 downto 0) after tdelay; -- set command 
             FRAME_N <= '0';-- after tdelay;                                  
             IRDY_N <= '1' after tdelay;
             parity_flag <= false;
             parity_read <= 'Z' after tdelay;    
             -- calculate  parity of address cycle
             parity(address(31 downto 0), bus_cmd(3 downto 0), parity_temp);
             parity_now <= parity_temp;        

             if GNT_N'Last_Event <= tsetup then  -- GNT setup time violation ?
                report "GNT setup time violation"
                severity Warning;
             end if; 
            wait until FALLING_EDGE(CLK);        --  make turnarround cycle 
             -- GM
             REQ_N <= '1';
             -- END GM      
             IRDY_N <= '0' after tdelay;    

             AD_Bus(31 downto 0) <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" after tdelay;
             C_BE_Bus(3 downto 0) <= bus_sel(1) after tdelay;  
             parity_now <= 'Z';     
            wait until FALLING_EDGE(CLK);       -- end turnarround cycle              

            -- wait for DEVSEL_N = '0' 
            -- (implement MASTER-ABORT if TARGET is not responding)
               -- wait for the number of IRDY state
             while ((start_irdy > 0) and (DEVSEL_N = '1' or DEVSEL_N = 'H' or 
                DEVSEL_N = 'Z' or DEVSEL_N = 'X') ) loop
                start_irdy := start_irdy -1;   -- from the begining of read cycle
                AD_Bus(31 downto 0) <="ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" after tdelay;
                C_BE_Bus(3 downto 0) <= bus_sel(1) after tdelay;  
               wait until FALLING_EDGE(CLK);                       
             end loop;
             --- exit cycle if RST_N or STOP_N are active           
             if RESET =1 or STOP_N = '0' or STOP_N ='L' then       
                stop := true;                   -- exit cycle
             end if;

             if DEVSEL_N'Last_Event <= tsetup then    
                report "DEVSEL_N  setup time violation"
                   severity Warning;
             end if; 

             --- inset IRDY_N if loop_irdy =1 and nr_irdy >1   
             if loop_irdy =1 then
                while  (nr_irdy > 1) loop
                   IRDY_N <='1' after tdelay;    
                   nr_irdy   := nr_irdy -1 ;
                  wait until FALLING_EDGE(CLK);                       
                end loop;  
                IRDY_N <='0' after tdelay;    
             end if;          
   
             -- terminate cycle because DEVSEL_N is anactive 
             -- (implement command MASTER-ABORT TERMINATION )   
             if start_irdy = 0 then   -- terminate cycle
                stop := true;
                FRAME_N <='1'; -- after tdelay;                                  
             end if;
     
             --   wait until FALLING_EDGE(CLK);                       
             -- repeat until all data are read or  STOP_N ='0'
             while ((data_number > 0) and (stop = false)) loop  -- read cycle  
                parity_flag <= true;
                data_number := data_number -1;
                parity_flag <= true;
                data_read := data(data_nr - data_number);
                if data_number = 0 then
                   C_BE_Bus(3 downto 0) <=bus_sel(data_nr) after tdelay; 
                   data_read := data(data_nr);      
                else 
                   C_BE_Bus(3 downto 0) <=bus_sel(data_nr - data_number+1) after tdelay;
                   data_read := data(data_nr - data_number);
                end if; 

                parity_flag <= true;
                if data_number =0 then
                   FRAME_N <='1';-- after tdelay;                           
                end if;

                -- exit cycle if RST_N or STOP_N are active
                if RESET = 1 or STOP_N = '0' or STOP_N = 'L' then       
                   stop := true;             -- exit cycle 
                   irdy_insert := false;
                end if;

                if irdy_insert = true and (data_nr - data_number) = loop_irdy  then
                   -- insert IRDY ='1' if insert_trdy =true 
                   while nr_irdy > 1 loop
                      nr_irdy   := nr_irdy -1 ;
                      IRDY_N <= '1' after tdelay;    -- insert IRDY
                      -- exit cycle if RST_N or STOP_N are active
                      if RESET = 1 or STOP_N = '0' or STOP_N = 'L' then       
                         stop := true;               -- exit cycle
                         nr_irdy := 1;
                      end if;                    
                     wait until FALLING_EDGE(CLK);
                   end loop; -- end loop nr_irdy
                end if; -- end if irdy_insert                         
                    
                --  IRDY_N <= '0' after tdelay; 
                if data_number = 0 then
                   IRDY_N <= '0' after tdelay;       
                end if;        
               wait until RISING_EDGE(CLK);
    
                if TRDY_N'Last_Event <= tsetup then    -- end of cycle
                   report "TRDY_N setup time violation"
                      severity Warning;
                end if;
       
                -- wait for TRDY_N = '0' (maxim 8 clock pulse)       
                while TRDY_N = '1' and trdy_exit = false loop      
                   wait until RISING_EDGE(CLK);
                   -- wait maxim 8 clock pulse if TRDY is asserted
                    trdy_stop := trdy_stop -1;   
                    if trdy_stop = 0 then
                       stop := true;
                       report "Target is not responding "
                          severity Warning;
                       FRAME_N <= '1';-- after tdelay;  
                       IRDY_N <= '1' after tdelay;
                       trdy_exit := true;
                    end if; 

                    -- exit cycle if RST_N or STOP_N are active
                    if RESET = 1 or STOP_N = '0' or STOP_N = 'L' then      
                       stop := true;                        -- exit cycle
                       trdy_exit := true;
                    end if;                         
                end loop; -- end loop TRDY_N=1                  

                Vec2Hex (data_read,str8,Good2);
                report "        EXPECTED DATA= "&str8 &"";  -- display expected data   

                Vec2Hex (AD_Bus,str8,Good2);
                report "        READ DATA= "&str8 &"";      -- display receive data

                if  AD_Bus'Last_Event <= tsetup then        -- end of cycle
                    report "AD_BUS setup time violation"
                       severity Warning;
                end if;
                   
                if ((data_number >0) and (stop =false)) then
                   wait until FALLING_EDGE(CLK);
                end if;

               wait for thold;
                if AD_Bus'Last_Event <= thold then    -- end of cycle
                   report "AD_Bus hold time violation"
                      severity Warning;
                end if;             
             end loop; -- end loop data_number

             if RESET =1 or STOP_N ='0' or STOP_N ='L' then      
                AD_Bus(31 downto 0) <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" after tdelay;
                C_BE_Bus(3 downto 0) <= "ZZZZ" after tdelay;  
                FRAME_N <= 'Z';-- after tdelay;                                  
                IRDY_N <= 'Z' after tdelay;
                parity_now <= 'Z' after tdelay;
                REQ_N <= 'H' after tdelay;
                parity_flag <= false;
             end if;                         
           else
           --------------------------------------------------------------------
           -- BEGIN WRITE CYCLE                                              --
           --------------------------------------------------------------------
              -- adress phase 
              parity_flag <= false;
              nr_irdy := nr_irdy;
              AD_Bus(31 downto 0) <= address(31 downto 0) after tdelay; -- set address
              C_BE_Bus(3 downto 0) <= bus_cmd(3 downto 0)after tdelay;  -- set command
              FRAME_N <= '0'; -- after tdelay;
              IRDY_N <= '1' after tdelay;
              parity_read <= 'Z';      
              parity(address(31 downto 0), bus_cmd(3 downto 0), parity_temp); -- calculate  parity of address cycle
              parity_now <= parity_temp after tdelay;
             wait until FALLING_EDGE(CLK);
              -- end adress phase                

              REQ_N <='1';

              parity_flag <= false;
              IRDY_N <= '0' after tdelay;    
              --  wait for 1 ns;
              -- wait for DEVSEL_N = '0' 
              -- (implement command MASTER-ABORT TERMINATION if TARGET not respond)
              while ((start_irdy > 0) and (DEVSEL_N ='H' or DEVSEL_N ='Z' or DEVSEL_N ='X') ) loop           -- wait for the number of IRDY state
                 start_irdy := start_irdy -1;        -- from the begining of read cycle  
                wait until FALLING_EDGE(CLK);
              end loop;

              -- if device not respond then exits the write command
              if start_irdy = 0 then   -- terminate cycle
                 stop := true;
                 FRAME_N <= '1';-- after tdelay;                                  
              end if;

              if DEVSEL_N'Last_Event <= tsetup then    
                 report "DEVSEL_N  setup time violation"
                    severity Warning;
              end if; 

              while ((data_number >0) and (stop =false)) loop
                 -- data phase
                 data_number := data_number -1;       
                 AD_Bus(31 downto 0) <= data(data_nr - data_number) after tdelay;      -- set DATA
                 C_BE_Bus(3 downto 0) <= bus_sel(data_nr - data_number) after tdelay;  -- set BE#s
                 IRDY_N <= '0' after tdelay;
                 parity(data(data_nr - data_number), bus_sel(data_nr - data_number), parity_temp); -- calculate  parity
                 parity_now <= parity_temp after tdelay;
                 -- end data phase
            
                 if data_number = 0 then
                    FRAME_N <= '1';-- after tdelay;
                 end if;    

                 -- exit cycle if RST_N,GNT_N or STOP_N are active
                 if RESET = 1 or STOP_N= '0' or STOP_N = 'L' then       
                    stop := true;                                -- exit cycle
                 end if;

                wait until RISING_EDGE(CLK);

                 if TRDY_N'Last_Event <= tsetup then    -- end of cycle
                    report "TRDY_N setup time violation"
                       severity Warning;
                 end if;

                 -- wait for TRDY_N ='0'       
                 while TRDY_N ='1' and   trdy_exit = false loop
                    wait until RISING_EDGE(CLK); 
                     -- wait maxim 8 clock pulse if TRDY is asserted
                     trdy_stop := trdy_stop -1;
                     if trdy_stop = 0 then
                        stop := true;
                        report "Target is not responding "
                           severity Warning;
                        FRAME_N <= '1';-- after tdelay;  
                        IRDY_N <= '1' after tdelay;
                        trdy_exit := true;                                
                     end if;                       

                     -- exit cycle if RST_N,GNT_N or STOP_N are active
                     if RESET = 1 or STOP_N = '0' or STOP_N = 'L' then       
                        stop := true;                       -- exit cycle
                        trdy_exit := true;
                     end if;
                 end loop; -- end loop TRDY=1            
      
                 if ((data_number > 0)  and (stop = false)) then
                    wait until FALLING_EDGE(CLK); -- synchronize with PCICLK
                 end if;
                 -- insert IRDY_N                               
                 if irdy_insert = true and (data_nr -data_number) = loop_irdy  
                            and data_number >0 and (stop =false) then 
                    while nr_irdy > 1 loop  
                       IRDY_N <='1' after tdelay;                                                                                    
                       nr_irdy := nr_irdy -1 ;   
                       if nr_irdy > 0 then
                          if data_number > 0 then
                             C_BE_Bus(3 downto 0) <=bus_sel(data_nr - data_number +1) after tdelay;  -- set BE#s
                             parity(data(data_nr - data_number), bus_sel(data_nr - data_number+1), parity_temp); -- calculate  parity        
                             parity_now <= parity_temp after tdelay;          
                          end if;
                       wait until FALLING_EDGE(CLK);
                       end if;

                       if RESET = 1 or STOP_N = '0' or STOP_N = 'L' then       
                          stop := true;                               -- exit cycle
                          nr_irdy := 1;
                       end if;                   
                    end loop;  -- end loop nr_irdy                                 
                 end if;         
                 report "        WRITE DATA= "&str8 &"";      -- display write data
            end loop; -- end loop start_irdy > 0 ...

            if RESET = 1 or STOP_N = '0' or STOP_N = 'L' then      
               AD_Bus(31 downto 0) <= "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" after tdelay;
               C_BE_Bus(3 downto 0) <= "ZZZZ" after tdelay;  
               FRAME_N <= 'Z';-- after tdelay;                                  
               IRDY_N <= 'Z' after tdelay;
               parity_now <= 'Z' after tdelay;
               REQ_N <= 'H' after tdelay;
               parity_flag <= false;
            end if;                                    
          end if; -- end rd_wr = '1'
       end if; -- if RST_N ='0' then
     end READ_WRITE;
     --------------------------------------------------------------------------
     --  Procedure implement instruction WRSW                                --
     --------------------------------------------------------------------------
     -- Writes single DWORD to memory space!
     procedure WRSW(
        data        : in    Data_buffer;
        address     : inout STD_LOGIC_VECTOR(31 downto 0);
        data_number : in    Integer;
        bus_sel     : in    Data_Enable) is
        variable   bus_cmd  : STD_LOGIC_VECTOR(3 downto 0);
        variable str8,str_8 : string(1 to 8);
        variable Good2      : Boolean;
     begin
        bus_cmd := "0111";
        address(1 downto 0) := "00";
        Vec2Hex (data(1),str8,Good2);
        Vec2Hex (address,str_8,Good2);      
        report "Write single DWORD to memory space! Address : "&str_8;
        READ_WRITE(address,data,1,bus_cmd,bus_sel,'0');
     end WRSW;
     --------------------------------------------------------------------------
     --  Procedure implement instruction RDSW                                --
     --------------------------------------------------------------------------
     --  Reads single DWORD from memory space!
     procedure RDSW(
        data        : in    Data_buffer;
        address     : inout STD_LOGIC_VECTOR(31 downto 0);
        data_number : in    Integer;
        bus_sel     : in    Data_Enable)   is
        variable bus_cmd    : STD_LOGIC_VECTOR(3 downto 0);
        variable read_data  : Data_buffer;
        variable str8,str_8 : string(1 to 8);
        variable Good2      : Boolean;
     begin
        bus_cmd := "0110";
        address(1 downto 0) := "00";  
        Vec2Hex (address,str_8,Good2);
        report "Read single DWORD from memory space! Address : "&str_8 ;
        READ_WRITE(address,data,1,bus_cmd,bus_sel,'1');
     end RDSW;
     --------------------------------------------------------------------------
     -- Procedure implements instruction WRMW                                --
     --------------------------------------------------------------------------
     -- Writes multiple DWORDs to memory space - burst mode
     procedure WRMW(
        data        : in    Data_buffer;
        address     : inout STD_LOGIC_VECTOR(31 downto 0);
        data_number : in    Integer;
        bus_sel     : in    Data_Enable)    is
        variable   bus_cmd  : STD_LOGIC_VECTOR(3 downto 0);
        variable str8,str_8 : string(1 to 8);
        variable Good2      : Boolean;
     begin
        bus_cmd := "1111";
        address(1 downto 0) := "10";
        Vec2Hex (address,str_8,Good2);
        report "Write multiple DWORDs to memory space! Address : "&str_8 ;
        READ_WRITE(address,data,data_number,bus_cmd,bus_sel,'0');    
     end WRMW;
     --------------------------------------------------------------------------
     -- Procedure implements instruction RDMW                                --
     --------------------------------------------------------------------------
     -- Reads multiple DWORDs from memory space - burst mode
     procedure RDMW(
        data        : in    Data_buffer;
        address     : inout STD_LOGIC_VECTOR(31 downto 0);
        data_number : in    Integer;
        bus_sel     : in    Data_Enable)   is
        variable bus_cmd    : STD_LOGIC_VECTOR(3 downto 0);
        variable read_data  : Data_buffer;
        variable str8,str_8 : string(1 to 8);
        variable Good2      : Boolean;
     begin
        bus_cmd := "1100";
        address(1 downto 0) := "10";
        Vec2Hex (address,str_8,Good2);
        report "Read multiple DWORDs from memory space! Address : "&str_8;
        READ_WRITE(address,data,data_number,bus_cmd,bus_sel,'1'); 
     end RDMW;
     --------------------------------------------------------------------------
     -- Procedure implements instruction RDML                                --
     --------------------------------------------------------------------------
     -- Reads multiple DWORDs from memory space!
     procedure RDML(
        data        : in    Data_buffer;
        address     : inout STD_LOGIC_VECTOR(31 downto 0);
        data_number : in    Integer;
        bus_sel     : in    Data_Enable)   is
        variable   bus_cmd: STD_LOGIC_VECTOR(3 downto 0);
        variable   read_data: Data_buffer;
        variable str8,str_8 : string(1 to 8);
        variable Good2       : Boolean;
     begin
        bus_cmd := "1110";
        address(1 downto 0) := "00";
        Vec2Hex (address,str_8,Good2);
        report "Reads multiple DWORDs from memory space! Address : "&str_8;
        READ_WRITE(address,data,data_number,bus_cmd,bus_sel,'1');      
     end RDML;
     --------------------------------------------------------------------------
     -- Procedure implements instruction WCFG                                --
     --------------------------------------------------------------------------
     -- writes configuration
     procedure WCFG(
        data        : in    Data_buffer;
        address     : inout STD_LOGIC_VECTOR(31 downto 0);
        data_number : in    Integer;
        bus_sel     : in    Data_Enable)   is
        variable bus_cmd    : STD_LOGIC_VECTOR(3 downto 0);
        variable read_data  : Data_buffer;
        variable str8,str_8 : string(1 to 8);
        variable Good2      : Boolean;
     begin
        bus_cmd :="1011";
        address(1 downto 0) :="00";              -- linear incrementing
        Vec2Hex (address,str_8,Good2);
        report "Configuration write! Address : "&str_8;
        READ_WRITE(address,data,data_number,bus_cmd,bus_sel,'0');  
     end WCFG;
     --------------------------------------------------------------------------
     -- Procedure implements instruction RCFG                                --
     --------------------------------------------------------------------------
     -- reads configuration
     procedure RCFG(
        data        : in    Data_buffer;
        address     : inout STD_LOGIC_VECTOR(31 downto 0);
        data_number : in    Integer;
        bus_sel     : in    Data_Enable)   is
        variable bus_cmd    : STD_LOGIC_VECTOR(3 downto 0);
        variable read_data  : Data_buffer;
        variable str8,str_8 : string(1 to 8);
        variable Good2      : Boolean;
     begin
        bus_cmd :="1010";
        address(1 downto 0) :="00";
        Vec2Hex (address,str_8,Good2);
        report "Configuration read ! Address : "&str_8; 
        READ_WRITE(address,data,data_number,bus_cmd,bus_sel,'1');      
     end RCFG;
     --------------------------------------------------------------------------
     -- Procedure implements instruction WRIO                                --
     --------------------------------------------------------------------------
     -- writes data to IO port
     procedure WRIO(
        data        : in Data_buffer;
        address     : inout STD_LOGIC_VECTOR(31 downto 0);
        data_number : in Integer;
        bus_sel     : in Data_Enable)    is
        variable bus_cmd    : STD_LOGIC_VECTOR(3 downto 0);
        variable read_data  : Data_buffer;
        variable str8,str_8 : string(1 to 8);
        variable Good2      : Boolean;
     begin
        bus_cmd := "0011";
        address(1 downto 0) := "00";   
        Vec2Hex (address,str_8,Good2);
        report "Write DWORD to I/O space! Address : "&str_8;  
        READ_WRITE(address,data,data_number,bus_cmd,bus_sel,'0'); 
     end WRIO;
     --------------------------------------------------------------------------
     -- Procedure implements instruction RDIO                                --
     --------------------------------------------------------------------------
     -- reads data from IO port
     procedure RDIO(
        data        : in    Data_buffer;
        address     : inout STD_LOGIC_VECTOR(31 downto 0);
        data_number : in    Integer;
        bus_sel     : in    Data_Enable)   is
        variable bus_cmd    : STD_LOGIC_VECTOR(3 downto 0);
        variable read_data  : Data_buffer;
        variable str8,str_8 : string(1 to 8);
        variable Good2      : Boolean;
     begin
        bus_cmd :="0010";
        address(1 downto 0) :="00";
        Vec2Hex (address,str_8,Good2);
        report "Read DWORD from memory space! Address : "&str_8;  
        READ_WRITE(address,data,data_number,bus_cmd,bus_sel,'1');   
     end RDIO;
     --------------------------------------------------------------------------
     -- Procedure implements instruction WAIT                                --
     --------------------------------------------------------------------------
     -- waits 
     procedure CWAT(
        data        : in Data_buffer;
        address     : inout STD_LOGIC_VECTOR(31 downto 0);
        data_number : in Integer;
        bus_sel     : in Data_Enable)   is  
        variable bus_cmd    : STD_LOGIC_VECTOR(3 downto 0);
        variable str8,str_8 : string(1 to 8);
        variable Good2      : Boolean;
        variable byte7_6,byte5,byte4,byte3_2,byte1,byte0 : Integer;
        variable Char7_6,Char5,Char4,Char3_2,Char1,Char0 : Std_Logic_Vector(7 downto 0);
     begin
        Char7_6 := data(1)(31 downto 24);
        irdy_loop := Byte2Int(Char7_6) ;	
        Char5 := "0000" & data(1)(23 downto 20);
        irdy_nr := Byte2Int(Char5) + 1;
        Char4 := "0000" & data(1)(19 downto 16);
        irdy_start := Byte2Int(Char4);
        bus_cmd := "0001";
        if irdy_loop = 0 or irdy_nr = 0 then 
           irdy_insert := false;
        else
           irdy_insert := true;
        end if;
        READ_WRITE(address,data,data_number,bus_cmd,bus_sel,'0'); 
     end CWAT;
     --------------------------------------------------------------------------
     -- Procedure implements instruction EXIT                                --
     --------------------------------------------------------------------------
     -- ends the Master's activity by placing in HighZ all the lines
     procedure ABRT is  
     begin
        wait until FALLING_EDGE(CLK);    -- synchro cycle 
         Init;
        wait;
     end ABRT;
     --------------------------------------------------------------------------
     -- Procedure implements the commands file parser and sequencer          --
     --------------------------------------------------------------------------
     procedure Main_case is     
         variable Commands    : CommandArray(1 to 100);     
         variable NumCommands : Natural;
         variable Good        : Boolean;
         constant HeaderMsg   : String         := "Main process:";
         constant MsgSeverity : Severity_Level := Failure;
         variable ErrFlag     : Boolean;
         variable PC          : Integer;    
     begin
         -- Read/Parse the commands file
         FileParser (cmd_file,Commands,NumCommands,ErrFlag);
         data_last_read :=  FALSE;
         if ErrFlag then
            report HeaderMsg&"Errors found in COMMAND file. Execute halts!!!"
               severity Warning;
         else
            PC := 0;
           wait until RST_N = '1';
            while PC < NumCommands Loop
               if RESET = 1 then
                  RESET := 0;
                  PC := 0;
               end if;
               while RST_N = '0' loop         -- RESET signal is active
                  PC := 0;                    -- if RST_N ='0' restart simulation
                  REQ_N <= 'H';               -- request is tri-stated
                 wait until Rising_Edge(CLK); -- synchro wait
               end loop;
               if STOP_N = '0' then        
                  report HeaderMsg&"Wait until signal STOP_N ='0' !";
                 wait until STOP_N = '1';
               end if;
               REQ_N <= '0';
   
               PC := PC + 1;          
               case Commands(pc).command is
                  when "WRSW" =>
                        WRSW(Commands(pc).data,Commands(PC).addr,Commands(pc).data_nr,Commands(pc).Enable);
                  when "RDSW" =>
                        RDSW(Commands(pc).data,Commands(PC).addr,Commands(pc).data_nr,Commands(pc).Enable);
                  when "WRMW" =>
                        WRMW(Commands(pc).data,Commands(PC).addr,Commands(pc).data_nr,Commands(pc).Enable);
                  when "RDMW" =>
                        RDMW(Commands(pc).data,Commands(PC).addr,Commands(pc).data_nr,Commands(pc).Enable);
                  when "RDML" =>
                        RDML(Commands(pc).data,Commands(PC).addr,Commands(pc).data_nr,Commands(pc).Enable);
                  when "WCFG" =>
                        WCFG(Commands(pc).data,Commands(PC).addr,Commands(pc).data_nr,Commands(pc).Enable);
                  when "RCFG" =>
                        RCFG(Commands(pc).data,Commands(PC).addr,Commands(pc).data_nr,Commands(pc).Enable);
                  when "WRIO" =>
                        WRIO(Commands(pc).data,Commands(PC).addr,Commands(pc).data_nr,Commands(pc).Enable);
                  when "RDIO" =>
                        RDIO(Commands(pc).data,Commands(PC).addr,Commands(pc).data_nr,Commands(pc).Enable);
                  when "CWAT" =>
                        CWAT(Commands(pc).data,Commands(PC).addr,Commands(pc).data_nr,Commands(pc).Enable);
                  when "ABRT" => ABRT;
                  when others	 => null;
               end case;
            end loop;
         end if;
     end Main_Case;
  ---------------------------------------------------------------------
  -- MS32PCI_MAIN : process begins
  ---------------------------------------------------------------------
  begin
     Init;
     Main_case;
  end process;
end Behavior; --================= End of architecture ===============--
-----------------------------------------------------------------------
-- Revision list
-- Version     Author          Date           Changes
--
-- 0.1       Ovidiu Lupas   June 09, 2000     New model
-----------------------------------------------------------------------

