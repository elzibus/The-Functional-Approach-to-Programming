#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/custom.h>

#define Canvas_val(v) ((struct nk_command_buffer *) (v))

#define Context_val(v) ((struct nk_context *) (v))

#define Panel_val(p) ((struct nk_panel *) (p))

value ocaml_nk_get_region(value ctxv) {

  CAMLparam1(ctxv) ;

  value rect = caml_alloc_small (4 * Double_wosize, Double_array_tag);

  struct nk_rect size;
  struct nk_context *ctx = Context_val(ctxv) ;

  size = nk_window_get_content_region(ctx) ;
  
  Store_double_field(rect, 0, size.x) ;
  Store_double_field(rect, 1, size.y) ;
  Store_double_field(rect, 2, size.w) ;
  Store_double_field(rect, 3, size.h) ;

  CAMLreturn(rect) ;
}

value ocaml_nk_window_get_canvas(value ctxv) {
  
  CAMLparam1(ctxv) ;

  struct nk_context *ctx = Context_val(ctxv) ;
  struct nk_command_buffer *canvas = nk_window_get_canvas(ctx);
  
  CAMLreturn(canvas) ;
}

value ocaml_nk_layout_row_static(value ctxv, value height, value item_width, value cols) {
  
  CAMLparam4(ctxv, height, item_width, cols) ;
  
  struct nk_context *ctx = Context_val(ctxv) ;

  nk_layout_row_static(ctx, Double_val(height), Int_val(item_width), Int_val(cols)) ;
 
  CAMLreturn(Val_unit) ;
}

value ocaml_nk_layout_row_dynamic(value ctxv, value height, value cols) {
  
  CAMLparam3(ctxv, height, cols) ;
  
  struct nk_context *ctx = Context_val(ctxv) ;

  nk_layout_row_dynamic(ctx, Double_val(height), Int_val(cols)) ;
 
  CAMLreturn(Val_unit) ;
}

value ocaml_nk_end(value ctxv) {
  
  CAMLparam1(ctxv) ;
  
  struct nk_context *ctx = Context_val(ctxv) ;

  nk_end(ctx) ;
 
  CAMLreturn(Val_unit) ;
}

value ocaml_nk_button_label(value ctxv, value title) {
  
  CAMLparam2(ctxv, title) ;
  
  struct nk_context *ctx = Context_val(ctxv) ;

  int val = nk_button_label(ctx, String_val(title)) ;
  
  CAMLreturn(Val_int(val)) ;
}

value ocaml_nk_label(value ctxv, value label, value flags) {
  
  CAMLparam3(ctxv, label, flags) ;
  
  struct nk_context *ctx = Context_val(ctxv) ;

  nk_label(ctx, String_val(label), Int_val(flags)) ;
  
  CAMLreturn(Val_unit) ;
}

value ocaml_nk_option_label(value ctxv, value label, value active) {
  
  CAMLparam3(ctxv, label, active) ;
  
  struct nk_context *ctx = Context_val(ctxv) ;

  int ret = nk_option_label(ctx, String_val(label), Int_val(active) ) ;
  
  CAMLreturn(Val_int(ret)) ;
}


value ocaml_nk_property_int(value ctxv, value label, value int_ref, value step, value ipp) {
  
  int val ;
  
  CAMLparam5(ctxv, label, int_ref, step, ipp) ;
  
  struct nk_context *ctx = Context_val(ctxv) ;

  val = Long_val(Field(int_ref, 0)) ;
  
  nk_property_int(ctx, String_val(label), 0, &val, 100, Int_val(step), Double_val(ipp)) ;

  Store_field(int_ref, 0, Val_int(val)) ;
  
  CAMLreturn(Val_unit) ;
}

struct custom_operations nuklear_ops = {
  "fr.inria.caml.nuklear",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

value alloc_panel(void) {
  value v = alloc_custom(&nuklear_ops, sizeof(struct nk_panel), 0, 1);
  return v;
}

value ocaml_new_panel() {
  CAMLparam0() ;
  CAMLreturn(alloc_panel()) ;
}

value ocaml_nk_propertyi(value ctxv, value name, value min, value val, value max, value step, value fincpp) {
  
  CAMLparam5(ctxv, name, min, val, max) ;
  CAMLxparam2(step, fincpp) ;
  
  struct nk_context *ctx = Context_val(ctxv) ;

  int ret = nk_propertyi(ctx, String_val(name), Int_val(min), Int_val(val), Int_val(max), Int_val(step), Double_val(fincpp)) ;
  
  CAMLreturn(Val_int(ret)) ;
}


value ocaml_nk_combo_end(value ctxv) {

  CAMLparam1(ctxv) ;

  struct nk_context *ctx = Context_val(ctxv) ;

  nk_combo_end(ctx);
  
  CAMLreturn(Val_unit) ;
}

value ocaml_nk_begin(value ctxv, value title, value x, value y, value w, value h, value flags) {

  CAMLparam5(ctxv, title, x, y, w) ;
  CAMLxparam2(h, flags) ;

  struct nk_context *ctx = Context_val(ctxv) ;

  CAMLreturn(Val_int(nk_begin(ctx, String_val(title),
			      nk_rect(Double_val(x), Double_val(y), Double_val(w), Double_val(h)),
			      Int_val(flags)))) ;
}

void ocaml_gui(value ctxv) {
  
  static value *closure_f = NULL ;

  if(closure_f == NULL) {
    closure_f = caml_named_value("ocaml_gui") ;
  }
  
  caml_callback(*closure_f, ctxv) ;
}

void ocaml_nk_stroke_line(value canvasv, value thickness, value x1, value y1, value x2, value y2) {
  
  CAMLparam5(canvasv, thickness, x1, y1, x2) ;
  CAMLxparam1(y2) ;

  const struct nk_color draw_color = nk_rgb(10, 170, 170);

  struct nk_command_buffer *canvas = Canvas_val(canvasv) ;
  
  nk_stroke_line(canvas,
		 Double_val(x1), Double_val(y1),
		 Double_val(x2), Double_val(y2),
		 Double_val(thickness),
		 draw_color) ;
  
  CAMLreturn(Val_unit) ;
}

void ocaml_nk_stroke_arc(value canvasv, value thickness, value xc, value yc, value radius, value amin, value amax) {
  
  CAMLparam5(canvasv, thickness, xc, yc, radius) ;
  CAMLxparam2(amin, amax) ;

  const struct nk_color draw_color = nk_rgb(10, 170, 170);

  struct nk_command_buffer *canvas = Canvas_val(canvasv) ;
  
  nk_stroke_arc(canvas,
		Double_val(xc), Double_val(yc),
		Double_val(radius),
		Double_val(amin), Double_val(amax),
		Double_val(thickness),
		draw_color) ;
  
  CAMLreturn(Val_unit) ;
}

void ocaml_nk_stroke_curve(value canvasv,
			   value thickness,
			   value ax, value ay,
			   value c1x, value c1y,
			   value c2x, value c2y,
			   value bx, value by) {
  
  CAMLparam5(canvasv, thickness, ax, ay, c1x) ;
  CAMLxparam5(c1y, c2x, c2y, by, by) ;

  const struct nk_color draw_color = nk_rgb(10, 170, 170);

  struct nk_command_buffer *canvas = Canvas_val(canvasv) ;
  
  nk_stroke_curve(canvas,
		  Double_val(ax), Double_val(ay),
		  Double_val(c1x), Double_val(c1y),
		  Double_val(c2x), Double_val(c2y),
		  Double_val(bx), Double_val(by),
		  Double_val(thickness),
		  draw_color) ;
  
  CAMLreturn(Val_unit) ;
}

