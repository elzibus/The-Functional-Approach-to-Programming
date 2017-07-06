#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/custom.h>

#define Canvas_val(v) ((struct nk_command_buffer *) (v))

#define Context_val(v) ((struct nk_context *) (v))

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

void ocaml_nk_stroke_line(value canvasv, value thickness, value col, value p1, value p2) {
  
  CAMLparam5(canvasv, thickness, col, p1, p2) ;

  int r = Long_val(Field(col, 0)) ;
  int g = Long_val(Field(col, 1)) ;
  int b = Long_val(Field(col, 2)) ;
  
  double x1 = Double_field(p1, 0) ;
  double y1 = Double_field(p1, 1) ;

  double x2 = Double_field(p2, 0) ;
  double y2 = Double_field(p2, 1) ;
  
  const struct nk_color draw_color = nk_rgb(r,g,b);

  struct nk_command_buffer *canvas = Canvas_val(canvasv) ;
  
  nk_stroke_line(canvas,
		 x1, y1,
		 x2, y2,
		 Double_val(thickness),
		 draw_color) ;
  
  CAMLreturn(Val_unit) ;
}

void ocaml_nk_stroke_curve(value canvasv,
			   value thickness,
			   value color,
			   value pta,
			   value ptc1,
			   value ptc2,
			   value ptb) {
  
  CAMLparam5(canvasv, thickness, color, pta, ptc1) ;
  CAMLxparam2(ptc2, ptb) ;

  int r = Long_val(Field(color, 0)) ;
  int g = Long_val(Field(color, 1)) ;
  int b = Long_val(Field(color, 2)) ;

  const struct nk_color draw_color = nk_rgb(r,g,b) ;

  double xa = Double_field(pta, 0) ;
  double ya = Double_field(pta, 1) ;

  double xc1 = Double_field(ptc1, 0) ;
  double yc1 = Double_field(ptc1, 1) ;

  double xc2 = Double_field(ptc2, 0) ;
  double yc2 = Double_field(ptc2, 1) ;

  double xb = Double_field(ptb, 0) ;
  double yb = Double_field(ptb, 1) ;

  struct nk_command_buffer *canvas = Canvas_val(canvasv) ;
  
  nk_stroke_curve(canvas,
		  xa, ya,
		  xc1, yc1,
		  xc2, yc2,
		  xb, yb,
		  Double_val(thickness),
		  draw_color) ;
  
  CAMLreturn(Val_unit) ;
}

