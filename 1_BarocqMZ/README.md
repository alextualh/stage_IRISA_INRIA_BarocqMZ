# BarocqMZ

This readme contains : 
1. Get started with this micro-separation-kernel
2. How to install the needed compilers
2. Code size analysis and benchmark
3. Comparison between pure C and Barocq C implementation

# 1. Get started with this micro-separation-kernel

Here is a micro separation kernel, coded in barocq as much as possible. You can see barocq functions as a lib, part of the code. 

## How to compile ?

The barocq generate a C file in _build : br_kernel.c, where all the barocq is compiled in one C file, and a header of the same name is generated aside.

The rest of the kernel, as you can see in kernel/src/, consists of two assembly files and three C (and .h) files. These are compiled with br_kernel.h and drivers (timer, ...) to produce kernel.elf, the final executable file, using `CompCert C` RISC-V compiler.

All of this is performed by the command : ```make```

Launched from the root of the project, it will go in kernel to do a make, and also in zones, to make them too.

```make clean``` is available, and you can also do ```make gcc``` to compile using `riscv64-unknown-elf-gcc`, instead of **default `CompCert C` for RISC-V compiler**.

## How to lauch the kernel ?

Once the .elf generated, (three : kernel, zone0 and zone1.elf), you can launch a test situation with the command ```./scripts/qemu.sh```.
You'll see print made by the zones and what they are sending and receiving from each other.


# 2. How to install the needed compilers

(For Barocq, will need `opam repo add coq-released https://coq.inria.fr/opam/released && opam install menhir.20240715 && opam install coq.8.20.1`
and `cp .opam/switch_BAROCQ/bin/barocq .opam/switch_COMPCERT-RV32/compcert-rv32/bin/ -r` replaceing the name switch_NAME by you switch name)


## 2.1 How to install `CompCert C` for RISC-V compiler
How to install `CompCert C` for RISC-V compiler ?
(the goal is to have the riscv64 linker and the riscv32 compiler)

Sources : https://compcert.org/man/manual.pdf but are not going to follow that because we need a specific modified version : **compcert-ce**. So let's go :

1. How to do while we have other compcert versions ? Let's create a switch with the right rv32 version :
`opam switch create  compcert-ce-rv32 ocaml.4.14.2`

2. Then, do : 
`opam pin -y -b add https://gitlab.inria.fr/cchavano/compcert-ce.git#rv32-linux`
It's going to take the branch `rv32-linux` on the repo, which is the version with the right configuration.
Actually, what's change is the build field in the file `compcert-ce.opam`
(now it's : 
    "./configure"
    "rv32-linux"
    "-toolprefix"
    "riscv64-unknown-elf-")

Then it's ready, it's like doing **./configure -toolprefix riscv64-unknown-elf-  rv32-linux** and **make && make install**, as intructed in compcert doc, but here it is with opam as needed by barocq source files (makefile config and all...).

Notice that here is the detailed configuration of CompCert for RiscV 32 : 

**./configure -toolprefix riscv64-unknown-elf-  rv32-linux** 
```
CompCert configuration:
    Target architecture........... riscV
    Hardware model................ 32
    Application binary interface.. standard
    Endianness.................... little
    OS and development env........ linux
    C compiler.................... riscv64-unknown-elf-gcc -march=rv32imafd -mabi=ilp32d
    C preprocessor................ riscv64-unknown-elf-gcc -march=rv32imafd -mabi=ilp32d -U__GNUC__ -E
    Assembler..................... riscv64-unknown-elf-gcc -march=rv32imafd -mabi=ilp32d -c
    Assembler supports CFI........ true
    Assembler for runtime lib..... riscv64-unknown-elf-gcc -march=rv32imafd -mabi=ilp32d -c
    Linker........................ riscv64-unknown-elf-gcc -march=rv32imafd -mabi=ilp32d -no-pie
    Archiver...................... riscv64-unknown-elf-ar rcs
    Math library.................. -lm
    Build command to use.......... make
    Menhir API library............ /udd/atualham/.opam/4.14.2/lib/menhirLib
    The Flocq library............. local
    The MenhirLib library......... local
    Binaries installed in......... /usr/local/bin
    Shared config installed in.... /usr/local/share
    Runtime library provided...... true
    Library files installed in.... /usr/local/lib/compcert
                                   (../lib/compcert relative to compcert.ini)
    Man pages installed in........ /usr/local/share/man
    Standard headers provided..... true
    Standard headers installed in. /usr/local/lib/compcert/include
    Coq development will not be installed
```

## 2.2 How to install `riscv64-unknown-elf-gcc` compiler ?

1. Explained in readme of https://github.com/riscv-collab/riscv-gnu-toolchain#
2. On Inria machines go to `/home` and not in `~/` (the first is true local, the second is limited to 12G quota)
3. create your own `/home/my_opt` with rights and mkdir riscv to build in
4. Go in `my_opt/riscv`
5. Do `/home/riscv-gnu-toolchain/configure --prefix=/home/my_opt/riscv --with-arch=rv64gc --with-abi=lp64` to create the good makefile
6. From `my_opt/riscv` do make newlib, it last 30 minutes without parallelization. To activate para do `make -j$(nproc)` to use all the cores
For me gdb build didn't work but everything before were fine so `riscv64-unknown-elf-gcc` compiler is available in `/home/my_opt/bin`
7. Change you `~/.bash_profile` by adding the line `export PATH=/home/my_opt/riscv/bin:$PATH`, then you can call `riscv64-unknown-elf-gcc` from anywhere
8. Test with the command `riscv64-unknown-elf-gcc --version`, it should return : 
```
riscv64-unknown-elf-gcc () 15.1.0
Copyright (C) 2025 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```
Finished !


## 3. Code size analysis and benchmark

Several code analysis, mainly by size of different source and generated files.

### Basic code line counting

Following the sloc standard (Source Line Of Code) or more precisely the Physical SLOC (P‑SLOC) rule, in which we count each code line excepting : blank lines, comments and single braces.
Thus, import, include, constants definitions are counted. For Barocq files I've used `ocamlwc`, and the result is :
| lines | in source file | lines in v2 (communication inter-zone) |
|-----------|---------------------------|----------------------------|         
|    135     |  src/br_ecall.br         |229   |
|    45      |src/br_interface.br |          50 |
|    16      |src/br_kernel.br    |              16| 
|    58      |src/br_sched.br   |              58 |
|    158     |src/br_trap.br    |               161|
|   **412**   |             |                     **514**  |



And all other languages file are counted with cloc and the result is : 
| lines | in language |
|-------|----------|         
|  170  |    C     |
|  144  |  Assembly|
|  119  | C Header |
|**433**|          |

With the addition of communication between zone in the v2 only 27 lines have been added to those 433 of non barocq files, so 460 now (in v2). 

and finally, it's not counted in P-SLOC standard but notice that the .ld script and Makefile are 40 and 36 lines long (not counted in the following numbers).

So, before any transpilation or compilation of the source code, we have **845**  lines of code, and after the compilation of Barocq files toward C, 
we have : **881** lines in C generated from the 412 Barocq lines, so in total it's **1314** lines to be compilated with the risc V **gcc** compiler.
While, in **TinyMZ** there were **529**lines of code to be compilated with the risc V **gcc** compiler.
Here is a board to summarize :

| Metric                             | BarocqMZ raw (ie barocq files not compilled) | TinyMZ                   | BarocqMZ with barocq compilled in C |
|------------------------------------|------------------------------------------|------------------------------|------------------------------|
| **Physical sources lines**         |    845                                   |  529                         |   1314  |



### Advanced counting 

There is a quick comparison of key size and line counting metrics for **BarocqMZ** vs **TinyMZ** kernels.

| Metric / Command                                              | BarocqMZ with CompCert | BarocqMZ with GCC       | TinyMZ                     |
|---------------------------------------------------------------|------------------------|-------------------------|----------------------------|
| **`.text` (code) size**<br>`size _build/kernel.elf`           |                        | 7 672 bytes             | 2 720 bytes                |
| **`.data` + `.bss` (RAM static)**<br>`size _build/kernel.elf` |                        |688 + 1 416 = 2 104 bytes| 616 + 1 024 = 1 640 bytes  |
| **`.text` section length**<br>`readelf -S … \| grep .text`    |         5904           |6672 bytes               | 2720 bytes                 |
| **ELF file size**<br>`ls -lh _build/kernel.elf`               |        **18 K**        |43 K                     | 23 K                       |
| **Assembly lines in kernel.elf**                              |        2447            |  2447                   |  675                       |

The last row on assembly lines is obtained by taking the number of line displayed by objdump on kernel.elf.





## 4. Comparison between pure C and Barocq C implementation
To start, there are the two versions of one same function : `sched_yield`. As you can see the Barocq version is much more longer (42 lines vs 8 for TinyMZ version) as it respect several strong principles like purity (don't depend on a global state and don't modify it neither, ie no side effect), linear typing (a ressource has to be used once and only once) and other that we're going to precise.

<table>
  <tr>
    <th>TinyMZ pure C version</th>
    <th>BarocqMZ C generated from Barocq</th>
  </tr>
  <tr>
    <td>
<pre><code>void sched_yield(void)
{
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
  KERNEL.intrp_deleg = 0;
//
//
  CURRENT = KERNEL.next;
//
//
//
  KERNEL.deadline += KERNEL.next_quantum;
//
//  
//
//
//
  uintptr_t nextId = (CURRENT->id + 1) % N_ZONES;
// extraction of zones and current in new variables
// accessing the current zone in the array of zones
// reading the id of the acceded current zone
// conversion cast needed for % operator
// binding of a constant
// make the main calculation with % on u32
// finally affect the result in variable nextID
//
  KERNEL.next = &ZONES[nextId];
// Simpler in Barocq, next is a u64, not a pointer
//
  KERNEL.next_quantum = QUANTUM_CYC;
// modif in place with side effect VS copy in br
//       
  write_mtimecmp(KERNEL.deadline);
// kernel.deadline must be bind before param in br
//
// no return needed, made by side effect
}
</code></pre>
    </td>
    <td>
<pre><code>struct Br_interface_kernel *Br_sched_sched_yield(struct Br_interface_kernel *$p_ks)
{
  register unsigned long long $b0;
  register unsigned long long $b2;
  register unsigned long long $b3;
  register unsigned long long $b1;
  register struct Br_interface_zone **$b4;
  register unsigned long long $b5;
  register struct Br_interface_zone *$u_current_zone;
  register unsigned long long $b6;
  register unsigned int $u_curr_zone_id;
  register unsigned int $u_n_zones;
  register unsigned int $u_nextId32;
  register unsigned long long $u_nextId;
  register struct Br_interface_kernel *$u_ks;
  register unsigned long long $b7;
  register unsigned long long $u_x;
  //
  (*$p_ks).intrp_deleg = 0LL;
  $u_ks = $p_ks;
  //
  $b0 = (*$u_ks).next;
  (*$u_ks).current = $b0;
  $u_ks = $u_ks;
  //
  $b2 = (*$u_ks).deadline;
  $b3 = (*$u_ks).next_quantum;
  $b1 = $b2 + $b3;
  (*$u_ks).deadline = $b1;
  $u_ks = $u_ks;
  //
  $b4 = (*$u_ks).zones;
  $b5 = (*$u_ks).current;
  $u_current_zone = *($b4 + $b5);
  $b6 = (*$u_current_zone).id;
  $u_curr_zone_id = (unsigned int) $b6;
  $u_n_zones = (unsigned int) Br_interface_N_ZONES;
  $u_nextId32 = ($u_curr_zone_id + 1) % $u_n_zones;
  $u_nextId = (unsigned long long) $u_nextId32;
  //
  (*$u_ks).next = $u_nextId;
  $u_ks = $u_ks;
  //
  (*$u_ks).next_quantum = Br_interface_QUANTUM_CYC;
  $u_ks = $u_ks;  
  //
  $b7 = (*$u_ks).deadline;
  $u_x = Br_interface_write_mtimecmp($b7);
  //
  return $u_ks;
}
</code></pre>
  </tr> 
</table>

Let's detail a little bit more, for example, in the `sched_yield` function, that line is expressed in three different ways in direct C (TinyMZ), in Barocq, and in generated C (BarocqMZ). There are these version, let's compare :
| C in TinyMZ      | Barocq in BarocqMZ      | generated C in BarocqMZ   |
|---------------|--------------|----------------|
| CURRENT = KERNEL.next; | let ks = ks.current <- ks.next in |  \$b0 =(*$u_ks).next; |
| | |  (*$u_ks).current = $b0; |
| | |  $u_ks = $u_ks;           |

This "action" of modifying the current zone in order to memorize that now it's the next which is the current one shows a lot of the major differences between TinyMZ and BarocqMZ micro kernels : 
1. side effect VS copy or pseudo pure copy
2. direct memory managment VS index in an array as pseudo-pointer
3. what knowledge we have in addition ? deterministic state in barocq ? idk

--> Maybe relevant : compare assembly generated, see what we lose, what we gain in term of properties, memory reliability / masterisation, idk...

### ASSEMBLY BAROCQ : 
200103ce <Br_sched_sched_yield>:
200103ce:	7139                	addi	sp,sp,-64
200103d0:	de06                	sw	ra,60(sp)
200103d2:	dc22                	sw	s0,56(sp)
200103d4:	da26                	sw	s1,52(sp)
200103d6:	d84a                	sw	s2,48(sp)
200103d8:	d64e                	sw	s3,44(sp)
200103da:	d452                	sw	s4,40(sp)
200103dc:	d256                	sw	s5,36(sp)
200103de:	d05a                	sw	s6,32(sp)
200103e0:	ce5e                	sw	s7,28(sp)
200103e2:	0080                	addi	s0,sp,64
200103e4:	fca42623          	sw	a0,-52(s0)
200103e8:	fcc42783          	lw	a5,-52(s0)
200103ec:	4681                	li	a3,0
200103ee:	4701                	li	a4,0
200103f0:	d794                	sw	a3,40(a5)
200103f2:	d7d8                	sw	a4,44(a5)
200103f4:	fcc42483          	lw	s1,-52(s0)
200103f8:	0104a903          	lw	s2,16(s1)
200103fc:	0144a983          	lw	s3,20(s1)
20010400:	0124a023          	sw	s2,0(s1)
20010404:	0134a223          	sw	s3,4(s1)
20010408:	0084a903          	lw	s2,8(s1)
2001040c:	00c4a983          	lw	s3,12(s1)
20010410:	0184aa03          	lw	s4,24(s1)
20010414:	01c4aa83          	lw	s5,28(s1)
20010418:	01490733          	add	a4,s2,s4
2001041c:	86ba                	mv	a3,a4
2001041e:	0126b6b3          	sltu	a3,a3,s2
20010422:	015987b3          	add	a5,s3,s5
20010426:	96be                	add	a3,a3,a5
20010428:	87b6                	mv	a5,a3
2001042a:	893a                	mv	s2,a4
2001042c:	89be                	mv	s3,a5
2001042e:	0124a423          	sw	s2,8(s1)
20010432:	0134a623          	sw	s3,12(s1)
20010436:	0304aa03          	lw	s4,48(s1)
2001043a:	0004a903          	lw	s2,0(s1)
2001043e:	0044a983          	lw	s3,4(s1)
20010442:	87ca                	mv	a5,s2
20010444:	078a                	slli	a5,a5,0x2
20010446:	97d2                	add	a5,a5,s4
20010448:	0007a903          	lw	s2,0(a5)
2001044c:	00492983          	lw	s3,4(s2)
20010450:	00092903          	lw	s2,0(s2)
20010454:	89ca                	mv	s3,s2
20010456:	2a81a703          	lw	a4,680(gp) # 800002a8 <Br_interface_N_ZONES>
2001045a:	2ac1a783          	lw	a5,684(gp) # 800002ac <Br_interface_N_ZONES+0x4>
2001045e:	893a                	mv	s2,a4
20010460:	00198793          	addi	a5,s3,1
20010464:	0327f933          	remu	s2,a5,s2
20010468:	8b4a                	mv	s6,s2
2001046a:	4b81                	li	s7,0
2001046c:	0164a823          	sw	s6,16(s1)
20010470:	0174aa23          	sw	s7,20(s1)
20010474:	2a01a703          	lw	a4,672(gp) # 800002a0 <Br_interface_QUANTUM_CYC>
20010478:	2a41a783          	lw	a5,676(gp) # 800002a4 <Br_interface_QUANTUM_CYC+0x4>
2001047c:	cc98                	sw	a4,24(s1)
2001047e:	ccdc                	sw	a5,28(s1)
20010480:	0084a903          	lw	s2,8(s1)
20010484:	00c4a983          	lw	s3,12(s1)
20010488:	854a                	mv	a0,s2
2001048a:	85ce                	mv	a1,s3
2001048c:	3ecd                	jal	2001007e <Br_interface_write_mtimecmp>
2001048e:	87a6                	mv	a5,s1
20010490:	853e                	mv	a0,a5
20010492:	50f2                	lw	ra,60(sp)
20010494:	5462                	lw	s0,56(sp)
20010496:	54d2                	lw	s1,52(sp)
20010498:	5942                	lw	s2,48(sp)
2001049a:	59b2                	lw	s3,44(sp)
2001049c:	5a22                	lw	s4,40(sp)
2001049e:	5a92                	lw	s5,36(sp)
200104a0:	5b02                	lw	s6,32(sp)
200104a2:	4bf2                	lw	s7,28(sp)
200104a4:	6121                	addi	sp,sp,64
200104a6:	8082                	ret




### ASSEMBLY TINYMZ
20010478 <sched_yield>:
20010478:	1101                	addi	sp,sp,-32
2001047a:	ce06                	sw	ra,28(sp)
2001047c:	cc22                	sw	s0,24(sp)
2001047e:	1000                	addi	s0,sp,32
20010480:	00018793          	mv	a5,gp
20010484:	0207a223          	sw	zero,36(a5)
20010488:	00018793          	mv	a5,gp
2001048c:	4b98                	lw	a4,16(a5)
2001048e:	00018793          	mv	a5,gp
20010492:	c398                	sw	a4,0(a5)
20010494:	00018793          	mv	a5,gp
20010498:	4798                	lw	a4,8(a5)
2001049a:	47dc                	lw	a5,12(a5)
2001049c:	00018693          	mv	a3,gp
200104a0:	4e88                	lw	a0,24(a3)
200104a2:	4ecc                	lw	a1,28(a3)
200104a4:	00a70633          	add	a2,a4,a0
200104a8:	8832                	mv	a6,a2
200104aa:	00e83833          	sltu	a6,a6,a4
200104ae:	00b786b3          	add	a3,a5,a1
200104b2:	00d807b3          	add	a5,a6,a3
200104b6:	86be                	mv	a3,a5
200104b8:	8732                	mv	a4,a2
200104ba:	87b6                	mv	a5,a3
200104bc:	00018693          	mv	a3,gp
200104c0:	c698                	sw	a4,8(a3)
200104c2:	c6dc                	sw	a5,12(a3)
200104c4:	00018793          	mv	a5,gp
200104c8:	439c                	lw	a5,0(a5)
200104ca:	439c                	lw	a5,0(a5)
200104cc:	0785                	addi	a5,a5,1
200104ce:	8b85                	andi	a5,a5,1
200104d0:	fef42623          	sw	a5,-20(s0)
200104d4:	fec42783          	lw	a5,-20(s0)
200104d8:	40f007b3          	neg	a5,a5
200104dc:	0f07f793          	andi	a5,a5,240
200104e0:	02078713          	addi	a4,a5,32
200104e4:	00018793          	mv	a5,gp
200104e8:	97ba                	add	a5,a5,a4
200104ea:	00878713          	addi	a4,a5,8
200104ee:	00018793          	mv	a5,gp
200104f2:	cb98                	sw	a4,16(a5)
200104f4:	00018693          	mv	a3,gp
200104f8:	6741                	lui	a4,0x10
200104fa:	4781                	li	a5,0
200104fc:	ce98                	sw	a4,24(a3)
200104fe:	cedc                	sw	a5,28(a3)
20010500:	00018793          	mv	a5,gp
20010504:	4798                	lw	a4,8(a5)
20010506:	47dc                	lw	a5,12(a5)
20010508:	853a                	mv	a0,a4
2001050a:	85be                	mv	a1,a5
2001050c:	36d1                	jal	200100d0 <write_mtimecmp>
2001050e:	0001                	nop
20010510:	40f2                	lw	ra,28(sp)
20010512:	4462                	lw	s0,24(sp)
20010514:	6105                	addi	sp,sp,32
20010516:	8082                	ret




_________________________________________________________________________________________________
# Reamde pre-filled model : 

## Suggestions for a good README

Every project is different, so consider which of these sections apply to yours. The sections used in the template are suggestions for most open source projects. Also keep in mind that while a README can be too long and detailed, too long is better than too short. If you think your README is too long, consider utilizing another form of documentation rather than cutting out information.

## Name
Choose a self-explaining name for your project.

## Description
Let people know what your project can do specifically. Provide context and add a link to any reference visitors might be unfamiliar with. A list of Features or a Background subsection can also be added here. If there are alternatives to your project, this is a good place to list differentiating factors.

## Badges
On some READMEs, you may see small images that convey metadata, such as whether or not all the tests are passing for the project. You can use Shields to add some to your README. Many services also have instructions for adding a badge.

## Visuals
Depending on what you are making, it can be a good idea to include screenshots or even a video (you'll frequently see GIFs rather than actual videos). Tools like ttygif can help, but check out Asciinema for a more sophisticated method.

## Installation
Within a particular ecosystem, there may be a common way of installing things, such as using Yarn, NuGet, or Homebrew. However, consider the possibility that whoever is reading your README is a novice and would like more guidance. Listing specific steps helps remove ambiguity and gets people to using your project as quickly as possible. If it only runs in a specific context like a particular programming language version or operating system or has dependencies that have to be installed manually, also add a Requirements subsection.

## Usage
Use examples liberally, and show the expected output if you can. It's helpful to have inline the smallest example of usage that you can demonstrate, while providing links to more sophisticated examples if they are too long to reasonably include in the README.

## Support
Tell people where they can go to for help. It can be any combination of an issue tracker, a chat room, an email address, etc.

## Roadmap
If you have ideas for releases in the future, it is a good idea to list them in the README.

## Contributing
State if you are open to contributions and what your requirements are for accepting them.

For people who want to make changes to your project, it's helpful to have some documentation on how to get started. Perhaps there is a script that they should run or some environment variables that they need to set. Make these steps explicit. These instructions could also be useful to your future self.

You can also document commands to lint the code or run tests. These steps help to ensure high code quality and reduce the likelihood that the changes inadvertently break something. Having instructions for running tests is especially helpful if it requires external setup, such as starting a Selenium server for testing in a browser.

## Authors and acknowledgment
Show your appreciation to those who have contributed to the project.

## License
For open source projects, say how it is licensed.

## Project status
If you have run out of energy or time for your project, put a note at the top of the README saying that development has slowed down or stopped completely. Someone may choose to fork your project or volunteer to step in as a maintainer or owner, allowing your project to keep going. You can also make an explicit request for maintainers.
