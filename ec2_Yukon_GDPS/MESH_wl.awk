 BEGIN { FS = "," }
{
  fecha = $1
  level = 0.04279286899 * $3 ** 0.69917350355

  printf ("%s,%s\n", fecha, level)

} END {

}
