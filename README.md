<a href="">
  <img src="img/colour-logo.png"
       alt="OCaml"
       style="border: none; width: 80px;" />
</a>


<h1 align="center"> 
    Turbo Katana 
</h1>
<h6 align="center">
An Object Oriented, Statically-Typed Programming Language. 
</h6>
<p align="center">
    <img src="img/logo.png" width="25%">
</p>



***

<br>
<br>

# Try it Yourself! 

## Dependencies

### Compiling the project locally

If you want to compile the project yourself, the following dependencies are required: 
- Opam: The Ocaml Package Manager
- Bison
- Flex
  
The rest of the dependencies are included in the `compil.opam` file an can be downloaded using the following command:

```
$ opam install . --deps-only
```

Then you can make sure everything works correctly by running the unitary tests using the following command:

```
$ dune runtest
```
If everything is well configured the command will display nothing. 

<br>

Thn you have to build the `Interp` in the `interprete` folder: 
```
$ make clean
$ make
``` 
This will allow you to execute your Turbo Katana programs.

### Using Docker
If you don't want to download any external dpendencies, you can use docker to pull the dev-image from the GitHub Container Registry. All you have to do is run the following command:
```
$ docker run --rm -it ghcr.io/oopsoverflow/katana:latest
```

***
<br>

## Running Your First Turbo Katana Program
Once you downloaded the required dependencies and configured the dev enviromment, you have to build the project: 
```
$ dune build
```
You can run the tests again to make sure everything went correctly. 
If so you can write your program and run it using the following command:
```
$ make katana <path-to-prog.kat-here>
```
For example Running the `gn.kat` program should give the following output:
```
$ make katana progs/gn.kat
```
```ocaml
>>> Running Turbo Katana v1.0.1.b
>>> Target progs/gn.kat
dune exec compilc progs/gn.kat | interprete/interp
Nombre de termes pour representer 33259 => 2
33 259
1 123 456 789
7 230
32 400
39 630
39 630
32 400
39 630
1
0
1 000 000
Somme des entiers de 1 195160 100:
5 050
Table des factorielles de 1 a 30 :
1!  = 1
2!  = 2
3!  = 6
4!  = 24
5!  = 120
6!  = 720
7!  = 5 040
8!  = 40 320
9!  = 362 880
10! = 3 628 800
11! = 39 916 800
12! = 479 001 600
13! = 6 227 020 800
14! = 87 178 291 200
15! = 1 307 674 368 000
16! = 20 922 789 888 000
17! = 355 687 428 096 000
18! = 6 402 373 705 728 000
19! = 121 645 100 408 832 000
20! = 2 432 902 008 176 640 000
21! = 51 090 942 171 709 440 000
22! = 1 124 000 727 777 607 680 000
23! = 25 852 016 738 884 976 640 000
24! = 620 448 401 733 239 439 360 000
25! = 15 511 210 043 330 985 984 000 000
26! = 403 291 461 126 605 635 584 000 000
27! = 10 888 869 450 418 352 160 768 000 000
28! = 304 888 344 611 713 860 501 504 000 000
29! = 8 841 761 993 739 701 954 543 616 000 000
30! = 265 252 859 812 191 058 636 308 480 000 000
```

***
<br>

## Platforms
This project is platform independent. It should run flawlessly on Linux, Windows and MacOS.

It was only tested on Windows and Linux nevertheless.

<br>

![Arch](https://img.shields.io/badge/Arch_Linux-1793D1?style=for-the-badge&logo=arch-linux&logoColor=white)
![Fedora](https://img.shields.io/badge/Fedora-294172?style=for-the-badge&logo=fedora&logoColor=white)
![Win](https://img.shields.io/badge/Windows-0078D6?style=for-the-badge&logo=windows&logoColor=white)
