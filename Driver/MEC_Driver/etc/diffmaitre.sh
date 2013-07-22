#!/bin/sh
# diffmaitre
#
# Affiche les differences entre un fichier et la version officielle
# utilisation:
# diffmaitre <fichier> [<option>]
# option ::= x|t|q|s|sq
#
# si option est absent ou option=x, on utilise xxdiff
# si option=t on utilise diff
# sinon on utilie diff -<option>

if [ "$1" = "" ]
then
echo Utilisation:
echo 'diffmaitre <fichier> [t|q|s|sq]'
exit
fi

# liste des repertoires de recherche
dir1=$ARMNLIB/modeles/GEMDM_shared/v_3.2.2/src
dir2=$ARMNLIB/modeles/PHY/v_4.4/src
dir3=$ARMNLIB/modeles/PHY/CLASS/v_3.0/src

# on va parcourir ces repertoires pour trouver la copie maitre
trouve=0
erreur=0
for i in $dir1 $dir2 $dir3
do
  if [ -f $i/$1 ]
  then
    # copie maitre trouvee:
    if [ "$2" = t ]
    then
      echo $i/$1
      diff $1 $i
    elif [ \( \( \( "$2" = q \) -o \( "$2" = s \) \) -o \( "$2" = sq \) \) -o \( "$2" = qs \) ]
    then
      diff -$2 $1 $i
    elif [ \( "$2" = "" \) -o \( "$2" = x \) ]
    then
      xxdiff $1 $i
    elif [ $erreur -eq 0 ]
    then
      echo "parametre $2 incorrect"
      erreur=1
    fi
  trouve=1
  fi
done
# On affiche un message si on a pas trouve de copie maitre
if [ $trouve -eq 0 ]
then
  echo Fichier $1 non trouve
fi
