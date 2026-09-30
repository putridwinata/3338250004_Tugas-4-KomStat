#1.Jika rata-rata pelanggan datang ke toko adalah 3 orang per jam, 
#modelkan dengan Poisson dan hitung P(X≥5).

#rata-rata pelanggan
lambda <- 3

#jumlah pelanggan
x_poisson <- 0:15

#peluang Poisson
pmf_poisson <- dpois(x_poisson, lambda = lambda)

#menampilkan peluang
data.frame(
  Jumlah_Pelanggan = x_poisson,
  Probabilitas = pmf_poisson
)

#grafik PMF Poisson
plot(x_poisson, pmf_poisson,
     type = "h",
     lwd = 3,
     main = "PMF Distribusi Poisson (lambda = 3)",
     xlab = "Jumlah pelanggan (X)",
     ylab = "P(X = x)")

#menghitung P(X >= 5)
# P(X >= 5) = 1 - P(X <= 4)
p_X_min_5 <- 1 - ppois(4, lambda = lambda)

p_X_min_5

#2.Dari 100 bola (20 berwarna merah), diambil 10 tanpa pengembalian. 
#Modelkan jumlah bola merah yang diambil dengan distribusi yang tepat

#parameter distribusi Hipergeometrik
N <- 100       # jumlah seluruh bola
K <- 20        # jumlah bola merah
n <- 10        # jumlah bola yang diambil

#jumlah (x)bola merah yang mungkin
x_hyper <- 0:n

#peluang Hipergeometrik
pmf_hyper <- dhyper(
  x_hyper,
  m = K,
  n = N - K,
  k = n
)

#menampilkan peluang
data.frame(
  Jumlah_Bola_Merah = x_hyper,
  Probabilitas = pmf_hyper
)

#grafik PMF Hipergeometrik
plot(x_hyper, pmf_hyper,
     type = "h",
     lwd = 3,
     main = "PMF Distribusi Hipergeometrik",
     xlab = "Jumlah bola merah yang diambil (X)",
     ylab = "P(X = x)")

#3.Simulasikan 1.000 percobaan Binomial (n=15,p=0.4)
#dan bandingkan histogram hasil simulasi dengan PMF teoretis.

#parameter distribusi Binomial
n_binom <- 15            #jumlah percobaan
p_binom <- 0.4           #peluang sukses
jumlah_simulasi <- 1000  #jumlah simulasi

#agar hasil tetap sama
set.seed(123)

#melakukan simulasi 1.000 percobaan
hasil_binom <- rbinom(
  jumlah_simulasi,
  size = n_binom,
  prob = p_binom
)

#nilai X(sukses) yang mungkin
x_binom <- 0:n_binom

#PMF Binomial teoritis
pmf_binom <- dbinom(
  x_binom,
  size = n_binom,
  prob = p_binom
)

#PMF teoritis
data.frame(
  Jumlah_Sukses = x_binom,
  PMF_Teoritis = pmf_binom
)

#histogram hasil simulasi
hist(hasil_binom,
     breaks = seq(-0.5, 15.5, by = 1),
     probability = TRUE,
     main = "Histogram Simulasi vs PMF Teoritis",
     xlab = "Jumlah keberhasilan (X)",
     ylab = "Probabilitas")

#titik PMF teoritis
points(x_binom, pmf_binom,
       pch = 19,col = "blue")

#garis PMF teoritis
lines(x_binom, pmf_binom,
      type = "h",
      lwd = 3,col = "brown")

#rata-rata hasil simulasi
mean_simulasi <- mean(hasil_binom)

#rata-rata teoritis
mean_teoritis <- n_binom * p_binom

mean_simulasi
mean_teoritis