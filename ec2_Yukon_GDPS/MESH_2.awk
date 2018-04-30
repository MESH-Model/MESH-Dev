 BEGIN { FS = " " 
   }
{
  if (NR == 1){
     printf ("%s %s %s %s ", $1, $2, $3, $NF)
  } else
     printf "%s ", $NF

  }
 END { printf "\n" }
