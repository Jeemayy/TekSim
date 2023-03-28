# Assignment 2 - Teknik Simulasi
## Membangkitkan Distribusi Geometrik dari RNG Additive
### Kelompok 4
### Jimy Munandar
### Ryan Mahardika
### Ahmad Reyhan Syaifullah


generate_geom <- function()
{
  # input parameter
  a <- as.integer(readline("Masukkan Nilai a: "))
  z0 <- as.integer(readline("Masukkan Nilai z0: "))
  c <- as.integer(readline("Masukkan Nilai c: "))
  m <- as.integer(readline("Masukkan Nilai m: "))
  n <- as.integer(readline("Masukkan Nilai n: "))
  p <- as.numeric(readline("Masukkan Nilai p: "))
  
  xi <- matrix(NA, n, 4)
  colnames(xi) <- c("aZ(i-1)+c", "Xi", "Ui", "Geom")
  for (i in 1:n)
  {
    Zn <- xi[i, 1] <- (a * z0 + c)    # membuat nilai pada kolom pertama (Zi = aZ0 + c)
    Xi <- xi[i, 2] <- xi[i, 1] %% m   # membuat nilai kolom kedua Zi*mod
    Ui <- xi[i, 3] <- xi[i, 2] / m    # membuat nilai uniform pada kolom ketiga
    z0 <- xi[i, 2]              # z0 akan diganti sampai perulangan berikutnya
    xi[i, 4] <- log(1-Ui)/log(1-p)    #Menghitung nilai distribusi Geom dengan dari tabel Ui
  }
  View(xi)
}

generate_geom()
