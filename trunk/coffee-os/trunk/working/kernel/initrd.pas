Unit Initrd;

// initrd -- Defines the interface for and structures relating to the initial ramdisk.
//             Written for JamesM's kernel development tutorials.
// and re-worked to CoffeeOS by Jazz.

interface

type

 Tinitrd_header=record
     nfiles:longword; // The number of files in the ramdisk.
 end;

 Tinitrd_file_header=record

     magic:char;     // Magic number, for error checking.
     name:array[1..64] of pchar;  // Filename.
     offset:longword;   // Offset in the initrd that the file starts.
     length:longword;   // Length of the file.
 end;

function initrd_read(node:Tfs_node;  offset:longword;  size:longword;  buffer:char):longword;
function initrd_readdir( node:Tfs_node;  index:longword): dirent ; //returns pointer)
function initrd_finddir(node:Tfs_node; name:string):Tfs_node; //returns pointer
function initialise_initrd(location:longword):Tfs_node; //returns pointer
procedure ListInitrd;

implementation

//the List Files function for InitRD
procedure ListInitrd;

var

  j,i:integer;
  node:^dirent;
  sz:longword;
  buf:array[1.256] of char;
  fsnode:^Tfs_node

begin

  i: = 0;
   node: = 0;
   
   while (node <> 0) do
   begin
       node = readdir_fs(fs_root, i))
       writeln('Found file ');
       writeln(node.name);
       fsnode := finddir_fs(fs_root, node.name);

       if ((fsnode.flags and $07) = FS_DIRECTORY)
       begin
           write('(directory)');
       end;
       else
       begin
           
           sz := read_fs(fsnode, 0, 256, buf);
           
           j:=0;
           repeat  
               monitor_put(buf[j]);
               inc(j);
			 until for (j > sz);
           writeline;
       end;
       inc(i);
   end;

end;


function initrd_read(node:Tfs_node;  offset:longword;  size:longword;  buffer:char):longword;

var

   header:Tinitrd_file_header;

begin
     header := file_headers[node.inode];
    if (offset > header.length) then
        initrd_read:=0;
    if (offset+size > header.length) then
        size := header.length-offset;
    move(buffer, (header.offset+offset), size);
    initrd_read:=size;
end;

function initrd_readdir(node:Tfs_node; index:longword): dirent ; //returns pointer)
begin
    if (node = initrd_root and index = 0)
    begin
      strcpy(dirent.name, 'dev');
      dirent.name[3] := 0;
      dirent.ino := 0;
      initrd_readdir:= dirent;
    end;

    if (index-1 >= nroot_nodes)
        initrd_readdir:= 0;
    strcpy(dirent.name, root_nodes[index-1].name);
    dirent.name[strlen(root_nodes[index-1].name)] := 0;
    dirent.ino = root_nodes[index-1].inode;
    initrd_readdir:= dirent;
end;

function  initrd_finddir(node:Tfs_node; name:pchar):Tfs_node; //returns pointer

var
  i:integer;

begin
    if (node = initrd_root and not strcmp(name, "dev") ) then
        initrd_finddir:= initrd_dev;
    i:=0;
    repeat
    
        if ( not strcmp(name, root_nodes[i].name)) then
            initrd_finddir:= root_nodes[i];
        inc(i);
    until i > nroot_nodes;
    initrd_finddir:=0;
end;

// Initialises the initial ramdisk. It gets passed the address of the multiboot module,
// and returns a completed filesystem node.
procedure initialise_initrd( location:longword):Tfs_node;

var
 initrd_header:Tinitrd_header;     // (pointer)    The header.
 file_headers:Tinitrd_file_header; // (pointer) The list of file headers.

 initrd_root:Tfs_node;             // (pointer)Our root directory node.
 initrd_dev:Tfs_node;              // (pointer)We also add a directory node for /dev, so we can mount devfs later on.
 root_nodes:Tfs_node;              // (pointer)List of file nodes.

 nroot_nodes:integer;                    // Number of file nodes.
 i:integer;

begin
    // Initialise the main and file header pointers and populate the root directory.
    initrd_header: = location;
    file_headers := (location+sizeof(Tinitrd_header));

    // Initialise the root directory.
    initrd_root := MemAlloc(sizeof(Tfs_node));
    move(initrd_root.name, 'initrd',sizeof(initrd_root.name));
    initrd_root.mask := 0;
    initrd_root.uid := 0; 
    initrd_root.gid := 0;
    initrd_root.inode := 0;
    initrd_root.length := 0;
    initrd_root.flags := FS_DIRECTORY;
    initrd_root.read := 0;
    initrd_root.write := 0;
    initrd_root.open := 0;
    initrd_root.close := 0;
    initrd_root.readdir := initrd_readdir;  //or = result of
    initrd_root.finddir := initrd_finddir;
    initrd_root.ptr := 0;
    initrd_root.impl := 0;

    // Initialise the /dev directory (required!)
    initrd_dev := Memmalloc(sizeof(Tfs_node));
      move(initrd_dev.name, 'dev',sizeof(initrd_dev.name));
    initrd_dev.mask := 0;
    initrd_dev.uid := 0;
    initrd_dev.gid :=0;
    initrd_dev.inode :=0;
    initrd_dev.length := 0;
    initrd_dev.flags := FS_DIRECTORY;
    initrd_dev.read := 0;
    initrd_dev.write := 0;
    initrd_dev.open := 0;
    initrd_dev.close := 0;
    initrd_dev.readdir := initrd_readdir;
    initrd_dev.finddir := initrd_finddir;
    initrd_dev.ptr := 0;
    initrd_dev.impl := 0;

    root_nodes := MemAlloc(sizeof (initrd_header.nfiles));
    nroot_nodes := initrd_header.nfiles;

    // For every file...
    i:=0;
    repeat 
    begin
        // Edit the file's header - currently it holds the file offset
        // relative to the start of the ramdisk. We want it relative to the start
        // of memory.
        file_headers[i].offset:=file_headers[i].offset + location;
        // Create a new file node.
        move(root_nodes[i].name, file_headers[i].name,sizeof(root_nodes[i].name));  //or result of file_headers[i].name
        root_nodes[i].mask := 0;
        root_nodes[i].uid := 0;
        root_nodes[i].gid := 0;
        root_nodes[i].length := file_headers[i].length;
        root_nodes[i].inode := i;
        root_nodes[i].flags := FS_FILE;
        root_nodes[i].read := initrd_read; //result of initrd_read
        root_nodes[i].write := 0;
        root_nodes[i].readdir := 0;
        root_nodes[i].finddir := 0;
        root_nodes[i].open := 0;
        root_nodes[i].close := 0;
        root_nodes[i].impl := 0;
    end;
    inc(i);
    until (i > initrd_header.nfiles);
    initialise_initrd:=initrd_root;
end;

end.
