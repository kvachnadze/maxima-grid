NF==1 && (!read_ijk) {
    n = 0
    nb=$1
    print nb
    next
}

NF==3{
    ni=$1
    nj=$2
    nk=$3
    ntot=ni*nj*nk
    read_ijk = 1
    RS="[ \t\n]+"
    next
}

{
    n ++ 
    if (n<=ni*nj*nk) print $1 > "x.dat.nb"nb
    else if (n<=2*ni*nj*nk) print $1 > "y.dat.nb"nb
    else if (n<3*ni*nj*nk) print $1 > "z.dat.nb"nb
    else {
	read_ijk=0
	RS="[\n]+"
    }
}
