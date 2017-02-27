n1 = c(1)
n11 = 29/81
n2 = c(1)
n22 = 12/81
n3 = c(12/14)
n33 = 14/81
n4 = c(4/7)
n44 = 7 / 81
n5 = c(11/19)
n55 = 19/81
w_entropy = 0
w_entropy = n11 * entropy_eval(n1) + n22 * entropy_eval(n2) + n33 * entropy_eval(n3) + n44 * entropy_eval(n4) + n55 * entropy_eval(n5)
w_entropy

n1 = c(0.421, 0.579)
n11 = 19/81
n2 = c(0.903, 0.097)
n22 = 62/81
w_entropy = n11 * entropy_eval(n1) + n22 * entropy_eval(n2)
w_entropy

p = kyphosis
presentp = (p == "present")*1
p1 = c(sum(presentp) / 81)
presentp = (p == "present")*1
p1 = c(sum(presentp) / 81)
presentp = (p == "present")*1
p1 = c(sum(presentp) / 81)

n1 = 64/81
entropy_eval(c(n1))
