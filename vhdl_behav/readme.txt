This models are written in VHDL!
Author is Ovidiu Lupas!

MASTER model
generates PCI compliant signals
checks Target signal compliance with PCI
checks data received from Target for correctness
generates assertion reports if Target signals are not PCI compliant

TARGET model
generates PCI compliant signals
checks Master signal compliance with PCI
checks data received from Master for correctness
generates assertion reports if Master signals are not PCI compliant

Description
The models are boardlevel simulation models and are useful in the testing phase
of
the PCI cores design. The models are 32 bit, 33 MHz PCI compliant but are easy
upgradable to 64 bit, 66 MHz. The models are free; you can redistribute them
and/or modify them under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
The models are distributed in the hope that they will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

Current Status:
design is available in VHDL from OpenCores CVS via <a
href="http://www.opencores.org/cvsweb.shtml/pci_core/vhdl_behav/">cvsweb</a>
documentation will be available in short time
if needed, easy upgradable to 64 bit, 66 MHz