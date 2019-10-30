#!/bin/bash
# create_runsvs_mod.sh
# This script creates runsvs_mod.ftn90 from list_of_bus_variables_used_by_svs.txt

NTILES=5
INFILE=$1
if [ "$2" == "" ]
then
	RUNSVSDIR=src
else
	RUNSVSDIR=$2
fi
RUNSVS_MOD=$RUNSVSDIR/runsvs_mod.ftn90
#
# Determine the number of variables
BUSNVAR=`cat $INFILE | wc -l`

# Ready to create the source code of the module
# ---------------------------------------------

cat >$RUNSVS_MOD <<EOF
MODULE runsvs_mod
IMPLICIT NONE
INTEGER, PARAMETER :: NG = NGRIDCELLS
INTEGER, PARAMETER :: runsvs_busnvar = $BUSNVAR
INTEGER, PARAMETER :: runsvs_ntiles = $NTILES
EOF
awk -F";" '
BEGIN{
	pos=1;
	printf("INTEGER, PARAMETER, DIMENSION(runsvs_busnvar) :: runsvs_buspos = (/")}
NR>1{
	printf(", ")}
{
	printf("(%d-1)*NG+1",pos);
	pos=pos+$5
}
END{
	printf("/)\n");
	printf("INTEGER, PARAMETER :: runsvs_busdim = %d*NG\n",pos)}
' $INFILE >> $RUNSVS_MOD
awk -F";" '
BEGIN{
        printf("INTEGER, PARAMETER, DIMENSION(runsvs_busnvar) :: runsvs_buslev = (/")}
NR>1{
        printf(", ")}
{
        printf("%d",$5)
}
END{
        printf("/)\n")}
' $INFILE >> $RUNSVS_MOD
awk -F";" '
BEGIN{
        printf("CHARACTER*20, PARAMETER, DIMENSION(runsvs_busnvar) :: runsvs_busvarname = (/")}
NR>1{
        printf(", ")}
{
        printf("\"%s\"",gensub(/ /,"","g",$1))
}
END{
        printf("/)\n")}
' $INFILE >> $RUNSVS_MOD
awk -F";" '
BEGIN{
        printf("CHARACTER*4, PARAMETER, DIMENSION(runsvs_busnvar) :: runsvs_busoutname = (/")}
NR>1{
        printf(", ")}
{
        printf("\"%s\"",$3)
}
END{
        printf("/)\n")}
' $INFILE >> $RUNSVS_MOD
awk -F";" '
BEGIN{
	pos=1}
{
        printf("INTEGER, PARAMETER :: %s = (%d-1)*NG+1\n",$1,pos);
	pos = pos+$5}
' $INFILE >> $RUNSVS_MOD
echo "END MODULE runsvs_mod" >> $RUNSVS_MOD
