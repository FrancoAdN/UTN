object versionTexto {
	method valoracion() = 3500
	method precio() = 5000 
}


object versionIlustrada {
	method valoracion() = 8000
	method precio() = 6000 
}


object fahrenheit451 inherits Libro(cant_paginas=null, editorial=null, genero=null, precio_base=null, autor = "Ray Bradbury"){
	var property version = versionTexto
	
	override method valoracion() = version.valoracion()
	override method precio() = version.precio()
}


object cuidadoConElPerro inherits Libro(cant_paginas=null, editorial=null, genero=null, precio_base=2900,autor = "Liliana Cinetto") {
	
	override method valoracion() = 5000 + self.precio() / 2
	
	override method precio() {
		if (precio_base.even()) {
			return precio_base + 500
		}
		return precio_base + 600
	}
}


object institutoMediciones {
	method calcular_valor_imaginario(likes) {
	  return (likes + 8) * 1.5
	}
}

object patronesDeDisenio inherits Libro(cant_paginas=null, editorial=null, genero=null, precio_base=null, autor="Erich Gamma") {
	var property cantidad_likes = 300

	
	method valor_imaginario () = institutoMediciones.calcular_valor_imaginario(cantidad_likes) 
	
	override method valoracion() = 4000 + self.valor_imaginario()*2
	override method precio() {
		if(self.valor_imaginario().even()){
			return 5000
		}
		return 6000
	}
}


object cristina {
	method le_gusta_el_libro(unLibro) {
		return (unLibro.precio() > 5500 || unLibro.autor() == "Liliana Cinetto")
	}
}


object roberto {
	method le_gusta_el_libro(unLibro) {
		return (unLibro.autor() == "Erich Gamma" || unLibro.autor() == "Jorge Bucay")
	}
}


object patricia {
	var property esta_de_humor
	var property precio_minimo_malhumor = 4000
	
	method le_gusta_el_libro(unLibro) {
		return (esta_de_humor || unLibro.precio() > precio_minimo_malhumor)
	}
}

object drama{
	method cumple_condicion_de_aptitud(unLibro, unaPersona){
		return (unLibro.valoracion()>4000 && unaPersona.compras().size().even())
	}
}

object ciencia{
	method cumple_condicion_de_aptitud(unLibro, _) = unLibro.editorial() == "Pearson"
}

object novela{
	method cumple_condicion_de_aptitud(unLibro, unaPersona){
		return unLibro.precio()<=unaPersona.dinero()
	}
}

class Libro {
	var property cant_paginas
	const property genero
	var property editorial
	var property valoracion_base = 3000
	var property precio_base
	var property autor
	const property reviews = []
	
	method valoracion() {
		if(cant_paginas > 150){
			return valoracion_base + 4000
		}
		return valoracion_base + 3000
	}
	
	method precio_extra() = 1000.min(5*cant_paginas)
	method precio() = (precio_base + self.precio_extra())
	

	method validar_review(unaReview) {
	  if (unaReview.puntaje() <= 0) {
		throw new DomainException(message="El puntaje debe ser positivo.") 
	  }

	  if (unaReview.comentario().length() < 10) {
		throw new DomainException(message="El comentario mayor a 10 caracteres.") 
	  }
	}
	method agregar_review(unaReview) {
		self.validar_review(unaReview)
		reviews.add(unaReview)
	}
	method reviews_grosas(){
		return reviews.filter({r => r.puntaje()>9})
	}
	method puntaje_promedio(){
		var puntaje_total = reviews.sum({r => r.puntaje()})
		return (puntaje_total/reviews.size())
	}
	method es_apto_para(unaPersona){
		return unaPersona.le_gusta_el_libro(self) && self.genero().cumple_condicion_de_aptitud(self, unaPersona)
	}
}

object patoruzu {
	var property cant_paginas = 35
	
	method es_apto_para(unaPersona) = true
}

class Review{
	var property puntaje
	var property comentario 
}

object innovador {
	method le_gusta_el_libro(unaPersona, unLibro) {
		
		return unaPersona.compras().isEmpty() || unaPersona.compras().last().libros().last().autor() != unLibro.autor()
	}
}

object tecnico {
	const property autores_conocidos =  ["Erich Gamma", "Jeremy Rifkin", "Martin Fowler"]
	method le_gusta_el_libro(unaPersona, unLibro) {
		return unLibro.cant_paginas().between(150, 300) && autores_conocidos.any({a => a == unLibro.autor()})
	}
}

object fanatico {
	var property autor_de_moda = "George R.R. Martin"

	method cambiar_autor_de_moda(autor) {
		autor_de_moda = autor;
	}
	method le_gusta_el_libro(unaPersona, unLibro) {
		return unLibro.autor() == autor_de_moda;
	}
}

class Persona {
	var property dinero = 1000
	var property valoracion_minima
	const property compras = []
	var property criterio 
	
	method cambiar_criterio(nuevo_criterio) {
	  criterio = nuevo_criterio
	}
	
	method le_gusta_el_libro(unLibro){
		return (unLibro.precio() <= dinero && unLibro.valoracion() > valoracion_minima && self.criterio().le_gusta_el_libro(self, unLibro))
	}
	method le_gustan_todos(librosList){
		return librosList.all({lib => self.le_gusta_el_libro(lib)})
	}
	method puede_pagar(unaCompra){
		return (unaCompra.precio()<=dinero)
	}
	method agregar_compra(unaCompra){
		if(not self.le_gustan_todos(unaCompra.libros()) || not self.puede_pagar(unaCompra)){
			throw new DomainException(message="No puede agregar la compra.")
		}
		compras.add(unaCompra)
	}
	method tiene_compra_cara(){
		return compras.any({c => c.precio()>8000})
	}
	method compras_mes_actual(){
		var hoy = new Date()
		return compras.filter({compra => compra.fecha().month()==hoy.month()})
	}
}


class CuponDescuento {
	var property codigo = ''
	var property dcto = 0
	
	method aplicar_dcto(precio_base){
		return precio_base - precio_base*dcto
	}
}


class Compra {
	const property libros = []
	var property cupon = new CuponDescuento()
	var property fecha = new Date()
	
	method precio_base(){
		return libros.sum({lib => lib.precio()})
	}
	method precio(){
		var precio_final = cupon.aplicar_dcto(self.precio_base())
		if(fecha.dayOfWeek()=='friday'){
			return precio_final*0.8
		}
		return precio_final
	}
	method libros_top(){
		return libros.filter({lib => lib.valoracion()>4000})
	}
	method autores(){
		return libros.map({lib => lib.autor()})
	}
}



class Obra inherits Libro {
	var property fasiculos = []
	
	override method precio_extra() {
		return fasiculos.map({lib => lib.precio_extra()}).sum()
	}
		method valoracion_extra() { 
		return (10 * cant_paginas.min(1000)) / 5;
	}
	
	override method valoracion() {
		return valoracion_base + self.valoracion_extra()
	}
	
	override method es_apto_para(unaPersona){
		return fasiculos.all({lib => lib.es_apto_para(unaPersona)})	
	}
	


}


