## Code developed by Marcos Angelini, Dick Brus and Gerard Heuvelink

set.seed(12345) # reproducible research

A = 2500000 # area of the stratum
n = 2300     # number of sampled segments within the stratum (by simple random sampling)
N = 175000  # total number of segments within the stratum
a_N = rnorm(n = N , mean = round(A/N,0), sd = 5) # area of the i segments for i = 1, 2, ..., n
A = sum(a_N) #sum of a
a_i = sample(a_N, replace = F, size = 2300)
# It would be nicer if you simulated a_i for all N segments so that you could compute the true t
# Because in that case you can also check if the true value is inside the confidence interval
# Repeating the procedure should have it inside 95% of the cases

index = rbinom(n = N,size = 1, prob = .7) # assume 70% of the segments well classified
t <- sum(a_N * index)
p <- t/A
index_i <- sample(x = index, replace = F, size = n)
y_i = a_i * index_i # y_i = 0 for wrongly classified; y_i = a_i segment correctly classified
pi_i = n/N      # probability of inclusion (equal for all segments within the stratum, since their area (a_i) were neglected)
hatt = sum(y_i/pi_i) # estimate of area well classified
hatp = hatt/A # proportion of area well classified

# Option 1
var_hatt = N^2 * var(y_i)/n # variance of hatt
var_hatp <- var_hatt/(A^2) # variance of hatp

# Confidence intervals
error = qt(0.975, df = n - 1) * sqrt(var_hatt) # confidence rage/2  
hatt.ul.CI = hatt + error # area upper limit confidence interval
hatt.ll.CI = hatt - error # area lower limit confidence interval

hatp.ll.CI <- hatt.ll.CI/A
hatp.ul.CI <- hatt.ul.CI/A

hatt.ul.CI; hatt; hatt.ll.CI
hatp.ul.CI; hatp; hatp.ll.CI  # need not be close to 70% because segment size not constant!

