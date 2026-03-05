import matplotlib.pyplot as plt
actual = 1 
anterior = 1
psi = []
while actual < 100000:
    psi.append(actual/anterior)
    pasador = anterior
    anterior = actual
    actual = actual + pasador

phi = (1 + 5**0.5) / 2 

plt.plot(psi)
plt.axhline(phi, linestyle="--", linewidth=1, label=f"phi ≈ {phi:.6f}")
plt.show()
   



