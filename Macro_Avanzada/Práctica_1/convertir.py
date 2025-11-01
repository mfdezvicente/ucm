from weasyprint import HTML, CSS

HTML(string="<h1>Hola</h1>").write_pdf(
    "salida.pdf",
    stylesheets=[CSS(string="@page { size: A4; margin: 15mm } body{font-family: sans-serif;}")]
)

# O desde archivo y con base_url para resolver rutas relativas a CSS/img:
HTML(filename="/Users/miguelfernandez/Desktop/git_ucm_clone/ucm/Macro_Avanzada/prueba1_ej3.ipynb", base_url=".").write_pdf("salida.pdf")
