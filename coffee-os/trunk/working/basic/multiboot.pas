unit multiboot;
 
interface
 
const
        KERNEL_STACKSIZE = $4000;
 
        MULTIBOOT_BOOTLOADER_MAGIC = $2BADB002;
 
type
        Pelf_section_header_table_t = ^elf_section_header_table_t;
        elf_section_header_table_t = packed record
          num: DWORD;
          size: DWORD;
          addr: DWORD;
          shndx: DWORD;
        end;
 
        Pmultibootinfo = ^multibootinfo;
        multibootinfo = packed record
          flags: DWORD;
          mem_lower: DWORD; { Amount of memory available below 1mb }
          mem_upper: DWORD; { Amount of memory available above 1mb }
          boot_device: DWORD;
          cmdline: DWORD;
          mods_count: DWORD;
          mods_addr: DWORD;
          elf_sec: elf_section_header_table_t;
          mmap_length: DWORD;
          mmap_addr: DWORD;
        end;
 
        Pmodule_t = ^module_t;
        module_t = packed record
          mod_start: DWORD;
          mod_end: DWORD;
          name: DWORD;
          reserved: DWORD;
        end;
 
        
  PMemoryMap = ^TMemoryMap;
  TMemoryMap = packed record
    Size: LongWord;
    BaseAddress: QWord;
    Length: QWord;
    MType: byte; //If not 1 then not available..byte?
  end;
 
implementation
 
end.
