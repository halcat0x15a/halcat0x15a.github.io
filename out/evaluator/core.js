// Compiled by ClojureScript 1.7.48 {}
goog.provide('evaluator.core');
goog.require('cljs.core');
goog.require('cljs.reader');
evaluator.core.self_evaluating_QMARK_ = (function evaluator$core$self_evaluating_QMARK_(exp){
return (exp === true) || (exp === false) || (typeof exp === 'number') || (typeof exp === 'string');
});
if(typeof evaluator.core.eval_form !== 'undefined'){
} else {
evaluator.core.eval_form = (function (){var method_table__5420__auto__ = cljs.core.atom.call(null,cljs.core.PersistentArrayMap.EMPTY);
var prefer_table__5421__auto__ = cljs.core.atom.call(null,cljs.core.PersistentArrayMap.EMPTY);
var method_cache__5422__auto__ = cljs.core.atom.call(null,cljs.core.PersistentArrayMap.EMPTY);
var cached_hierarchy__5423__auto__ = cljs.core.atom.call(null,cljs.core.PersistentArrayMap.EMPTY);
var hierarchy__5424__auto__ = cljs.core.get.call(null,cljs.core.PersistentArrayMap.EMPTY,new cljs.core.Keyword(null,"hierarchy","hierarchy",-1053470341),cljs.core.get_global_hierarchy.call(null));
return (new cljs.core.MultiFn(cljs.core.symbol.call(null,"evaluator.core","eval-form"),((function (method_table__5420__auto__,prefer_table__5421__auto__,method_cache__5422__auto__,cached_hierarchy__5423__auto__,hierarchy__5424__auto__){
return (function (env,exp){
return cljs.core.first.call(null,exp);
});})(method_table__5420__auto__,prefer_table__5421__auto__,method_cache__5422__auto__,cached_hierarchy__5423__auto__,hierarchy__5424__auto__))
,new cljs.core.Keyword(null,"default","default",-1987822328),hierarchy__5424__auto__,method_table__5420__auto__,prefer_table__5421__auto__,method_cache__5422__auto__,cached_hierarchy__5423__auto__));
})();
}
evaluator.core.eval = (function evaluator$core$eval(env,exp){
if(cljs.core.truth_(evaluator.core.self_evaluating_QMARK_.call(null,exp))){
return exp;
} else {
if((exp instanceof cljs.core.Symbol)){
return cljs.core.get.call(null,cljs.core.deref.call(null,env),cljs.core.munge.call(null,[cljs.core.str(exp)].join('')));
} else {
if(cljs.core.seq_QMARK_.call(null,exp)){
return evaluator.core.eval_form.call(null,env,exp);
} else {
return null;
}
}
}
});
cljs.core._add_method.call(null,evaluator.core.eval_form,new cljs.core.Symbol(null,"quote","quote",1377916282,null),(function (env,p__5791){
var vec__5792 = p__5791;
var _ = cljs.core.nth.call(null,vec__5792,(0),null);
var quotation = cljs.core.nth.call(null,vec__5792,(1),null);
return quotation;
}));
cljs.core._add_method.call(null,evaluator.core.eval_form,new cljs.core.Symbol(null,"if","if",1181717262,null),(function (env,p__5793){
var vec__5794 = p__5793;
var _ = cljs.core.nth.call(null,vec__5794,(0),null);
var predicate = cljs.core.nth.call(null,vec__5794,(1),null);
var consequent = cljs.core.nth.call(null,vec__5794,(2),null);
var alternative = cljs.core.nth.call(null,vec__5794,(3),null);
if(cljs.core.truth_(evaluator.core.eval.call(null,env,predicate))){
return evaluator.core.eval.call(null,env,consequent);
} else {
return evaluator.core.eval.call(null,env,alternative);
}
}));
cljs.core._add_method.call(null,evaluator.core.eval_form,new cljs.core.Symbol(null,"define","define",-366059178,null),(function (env,p__5796){
var vec__5797 = p__5796;
var _ = cljs.core.nth.call(null,vec__5797,(0),null);
var variable = cljs.core.nth.call(null,vec__5797,(1),null);
var value = cljs.core.nth.call(null,vec__5797,(2),null);
return cljs.core.swap_BANG_.call(null,env,((function (vec__5797,_,variable,value){
return (function (p1__5795_SHARP_){
return cljs.core.assoc.call(null,p1__5795_SHARP_,[cljs.core.str(variable)].join(''),evaluator.core.eval.call(null,env,value));
});})(vec__5797,_,variable,value))
);
}));
cljs.core._add_method.call(null,evaluator.core.eval_form,new cljs.core.Symbol(null,"begin","begin",1321497208,null),(function (env,p__5799){
var vec__5800 = p__5799;
var exps = cljs.core.nthnext.call(null,vec__5800,(0));
return cljs.core.last.call(null,cljs.core.map.call(null,((function (vec__5800,exps){
return (function (p1__5798_SHARP_){
return evaluator.core.eval.call(null,env,p1__5798_SHARP_);
});})(vec__5800,exps))
,exps));
}));

evaluator.core.Procedure = {};

evaluator.core.app = (function evaluator$core$app(f,args){
if((!((f == null))) && (!((f.evaluator$core$Procedure$app$arity$2 == null)))){
return f.evaluator$core$Procedure$app$arity$2(f,args);
} else {
var x__5162__auto__ = (((f == null))?null:f);
var m__5163__auto__ = (evaluator.core.app[goog.typeOf(x__5162__auto__)]);
if(!((m__5163__auto__ == null))){
return m__5163__auto__.call(null,f,args);
} else {
var m__5163__auto____$1 = (evaluator.core.app["_"]);
if(!((m__5163__auto____$1 == null))){
return m__5163__auto____$1.call(null,f,args);
} else {
throw cljs.core.missing_protocol.call(null,"Procedure.app",f);
}
}
}
});

cljs.core._add_method.call(null,evaluator.core.eval_form,new cljs.core.Keyword(null,"default","default",-1987822328),(function (env,p__5804){
var vec__5805 = p__5804;
var operator = cljs.core.nth.call(null,vec__5805,(0),null);
var operands = cljs.core.nthnext.call(null,vec__5805,(1));
return evaluator.core.app.call(null,evaluator.core.eval.call(null,env,operator),cljs.core.map.call(null,((function (vec__5805,operator,operands){
return (function (p1__5803_SHARP_){
return evaluator.core.eval.call(null,env,p1__5803_SHARP_);
});})(vec__5805,operator,operands))
,operands));
}));

/**
* @constructor
*/
evaluator.core.Lambda = (function (env,parameters,body){
this.env = env;
this.parameters = parameters;
this.body = body;
})
evaluator.core.Lambda.prototype.evaluator$core$Procedure$ = true;

evaluator.core.Lambda.prototype.evaluator$core$Procedure$app$arity$2 = (function (lambda,args){
var self__ = this;
var lambda__$1 = this;
return evaluator.core.eval.call(null,cljs.core.atom.call(null,cljs.core.merge.call(null,cljs.core.deref.call(null,self__.env),cljs.core.zipmap.call(null,self__.parameters,args))),self__.body);
});

evaluator.core.Lambda.getBasis = (function (){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"env","env",-175281708,null),new cljs.core.Symbol(null,"parameters","parameters",410611779,null),new cljs.core.Symbol(null,"body","body",-408674142,null)], null);
});

evaluator.core.Lambda.cljs$lang$type = true;

evaluator.core.Lambda.cljs$lang$ctorStr = "evaluator.core/Lambda";

evaluator.core.Lambda.cljs$lang$ctorPrWriter = (function (this__5105__auto__,writer__5106__auto__,opt__5107__auto__){
return cljs.core._write.call(null,writer__5106__auto__,"evaluator.core/Lambda");
});

evaluator.core.__GT_Lambda = (function evaluator$core$__GT_Lambda(env,parameters,body){
return (new evaluator.core.Lambda(env,parameters,body));
});

cljs.core._add_method.call(null,evaluator.core.eval_form,new cljs.core.Symbol(null,"lambda","lambda",157104302,null),(function (env,p__5806){
var vec__5807 = p__5806;
var _ = cljs.core.nth.call(null,vec__5807,(0),null);
var parameters = cljs.core.nth.call(null,vec__5807,(1),null);
var body = cljs.core.nth.call(null,vec__5807,(2),null);
return (new evaluator.core.Lambda(env,parameters,body));
}));
(evaluator.core.Procedure["function"] = true);

(evaluator.core.app["function"] = (function (f,args){
return cljs.core.apply.call(null,f,args);
}));

//# sourceMappingURL=core.js.map