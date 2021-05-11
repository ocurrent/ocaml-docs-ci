/*
  Highlight.js 10.7.2 (00233d63)
  License: BSD-3-Clause
  Copyright (c) 2006-2021, Ivan Sagalaev
*/
var hljs=function(){"use strict";function e(t){
return t instanceof Map?t.clear=t.delete=t.set=()=>{
throw Error("map is read-only")}:t instanceof Set&&(t.add=t.clear=t.delete=()=>{
throw Error("set is read-only")
}),Object.freeze(t),Object.getOwnPropertyNames(t).forEach((n=>{var i=t[n]
;"object"!=typeof i||Object.isFrozen(i)||e(i)})),t}var t=e,n=e;t.default=n
;class i{constructor(e){
void 0===e.data&&(e.data={}),this.data=e.data,this.isMatchIgnored=!1}
ignoreMatch(){this.isMatchIgnored=!0}}function s(e){
return e.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;").replace(/'/g,"&#x27;")
}function a(e,...t){const n=Object.create(null);for(const t in e)n[t]=e[t]
;return t.forEach((e=>{for(const t in e)n[t]=e[t]})),n}const r=e=>!!e.kind
;class l{constructor(e,t){
this.buffer="",this.classPrefix=t.classPrefix,e.walk(this)}addText(e){
this.buffer+=s(e)}openNode(e){if(!r(e))return;let t=e.kind
;e.sublanguage||(t=`${this.classPrefix}${t}`),this.span(t)}closeNode(e){
r(e)&&(this.buffer+="</span>")}value(){return this.buffer}span(e){
this.buffer+=`<span class="${e}">`}}class o{constructor(){this.rootNode={
children:[]},this.stack=[this.rootNode]}get top(){
return this.stack[this.stack.length-1]}get root(){return this.rootNode}add(e){
this.top.children.push(e)}openNode(e){const t={kind:e,children:[]}
;this.add(t),this.stack.push(t)}closeNode(){
if(this.stack.length>1)return this.stack.pop()}closeAllNodes(){
for(;this.closeNode(););}toJSON(){return JSON.stringify(this.rootNode,null,4)}
walk(e){return this.constructor._walk(e,this.rootNode)}static _walk(e,t){
return"string"==typeof t?e.addText(t):t.children&&(e.openNode(t),
t.children.forEach((t=>this._walk(e,t))),e.closeNode(t)),e}static _collapse(e){
"string"!=typeof e&&e.children&&(e.children.every((e=>"string"==typeof e))?e.children=[e.children.join("")]:e.children.forEach((e=>{
o._collapse(e)})))}}class c extends o{constructor(e){super(),this.options=e}
addKeyword(e,t){""!==e&&(this.openNode(t),this.addText(e),this.closeNode())}
addText(e){""!==e&&this.add(e)}addSublanguage(e,t){const n=e.root
;n.kind=t,n.sublanguage=!0,this.add(n)}toHTML(){
return new l(this,this.options).value()}finalize(){return!0}}function g(e){
return e?"string"==typeof e?e:e.source:null}
const u=/\[(?:[^\\\]]|\\.)*\]|\(\??|\\([1-9][0-9]*)|\\./,h="[a-zA-Z]\\w*",d="[a-zA-Z_]\\w*",f="\\b\\d+(\\.\\d+)?",p="(-?)(\\b0[xX][a-fA-F0-9]+|(\\b\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)",m="\\b(0b[01]+)",b={
begin:"\\\\[\\s\\S]",relevance:0},E={className:"string",begin:"'",end:"'",
illegal:"\\n",contains:[b]},x={className:"string",begin:'"',end:'"',
illegal:"\\n",contains:[b]},v={
begin:/\b(a|an|the|are|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such|will|you|your|they|like|more)\b/
},w=(e,t,n={})=>{const i=a({className:"comment",begin:e,end:t,contains:[]},n)
;return i.contains.push(v),i.contains.push({className:"doctag",
begin:"(?:TODO|FIXME|NOTE|BUG|OPTIMIZE|HACK|XXX):",relevance:0}),i
},y=w("//","$"),N=w("/\\*","\\*/"),R=w("#","$");var _=Object.freeze({
__proto__:null,MATCH_NOTHING_RE:/\b\B/,IDENT_RE:h,UNDERSCORE_IDENT_RE:d,
NUMBER_RE:f,C_NUMBER_RE:p,BINARY_NUMBER_RE:m,
RE_STARTERS_RE:"!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|-|-=|/=|/|:|;|<<|<<=|<=|<|===|==|=|>>>=|>>=|>=|>>>|>>|>|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~",
SHEBANG:(e={})=>{const t=/^#![ ]*\//
;return e.binary&&(e.begin=((...e)=>e.map((e=>g(e))).join(""))(t,/.*\b/,e.binary,/\b.*/)),
a({className:"meta",begin:t,end:/$/,relevance:0,"on:begin":(e,t)=>{
0!==e.index&&t.ignoreMatch()}},e)},BACKSLASH_ESCAPE:b,APOS_STRING_MODE:E,
QUOTE_STRING_MODE:x,PHRASAL_WORDS_MODE:v,COMMENT:w,C_LINE_COMMENT_MODE:y,
C_BLOCK_COMMENT_MODE:N,HASH_COMMENT_MODE:R,NUMBER_MODE:{className:"number",
begin:f,relevance:0},C_NUMBER_MODE:{className:"number",begin:p,relevance:0},
BINARY_NUMBER_MODE:{className:"number",begin:m,relevance:0},CSS_NUMBER_MODE:{
className:"number",
begin:f+"(%|em|ex|ch|rem|vw|vh|vmin|vmax|cm|mm|in|pt|pc|px|deg|grad|rad|turn|s|ms|Hz|kHz|dpi|dpcm|dppx)?",
relevance:0},REGEXP_MODE:{begin:/(?=\/[^/\n]*\/)/,contains:[{className:"regexp",
begin:/\//,end:/\/[gimuy]*/,illegal:/\n/,contains:[b,{begin:/\[/,end:/\]/,
relevance:0,contains:[b]}]}]},TITLE_MODE:{className:"title",begin:h,relevance:0
},UNDERSCORE_TITLE_MODE:{className:"title",begin:d,relevance:0},METHOD_GUARD:{
begin:"\\.\\s*[a-zA-Z_]\\w*",relevance:0},END_SAME_AS_BEGIN:e=>Object.assign(e,{
"on:begin":(e,t)=>{t.data._beginMatch=e[1]},"on:end":(e,t)=>{
t.data._beginMatch!==e[1]&&t.ignoreMatch()}})});function k(e,t){
"."===e.input[e.index-1]&&t.ignoreMatch()}function M(e,t){
t&&e.beginKeywords&&(e.begin="\\b("+e.beginKeywords.split(" ").join("|")+")(?!\\.)(?=\\b|\\s)",
e.__beforeBegin=k,e.keywords=e.keywords||e.beginKeywords,delete e.beginKeywords,
void 0===e.relevance&&(e.relevance=0))}function O(e,t){
Array.isArray(e.illegal)&&(e.illegal=((...e)=>"("+e.map((e=>g(e))).join("|")+")")(...e.illegal))
}function A(e,t){if(e.match){
if(e.begin||e.end)throw Error("begin & end are not supported with match")
;e.begin=e.match,delete e.match}}function L(e,t){
void 0===e.relevance&&(e.relevance=1)}
const I=["of","and","for","in","not","or","if","then","parent","list","value"]
;function j(e,t,n="keyword"){const i={}
;return"string"==typeof e?s(n,e.split(" ")):Array.isArray(e)?s(n,e):Object.keys(e).forEach((n=>{
Object.assign(i,j(e[n],t,n))})),i;function s(e,n){
t&&(n=n.map((e=>e.toLowerCase()))),n.forEach((t=>{const n=t.split("|")
;i[n[0]]=[e,B(n[0],n[1])]}))}}function B(e,t){
return t?Number(t):(e=>I.includes(e.toLowerCase()))(e)?0:1}
function T(e,{plugins:t}){function n(t,n){
return RegExp(g(t),"m"+(e.case_insensitive?"i":"")+(n?"g":""))}class i{
constructor(){
this.matchIndexes={},this.regexes=[],this.matchAt=1,this.position=0}
addRule(e,t){
t.position=this.position++,this.matchIndexes[this.matchAt]=t,this.regexes.push([t,e]),
this.matchAt+=(e=>RegExp(e.toString()+"|").exec("").length-1)(e)+1}compile(){
0===this.regexes.length&&(this.exec=()=>null)
;const e=this.regexes.map((e=>e[1]));this.matcherRe=n(((e,t="|")=>{let n=0
;return e.map((e=>{n+=1;const t=n;let i=g(e),s="";for(;i.length>0;){
const e=u.exec(i);if(!e){s+=i;break}
s+=i.substring(0,e.index),i=i.substring(e.index+e[0].length),
"\\"===e[0][0]&&e[1]?s+="\\"+(Number(e[1])+t):(s+=e[0],"("===e[0]&&n++)}return s
})).map((e=>`(${e})`)).join(t)})(e),!0),this.lastIndex=0}exec(e){
this.matcherRe.lastIndex=this.lastIndex;const t=this.matcherRe.exec(e)
;if(!t)return null
;const n=t.findIndex(((e,t)=>t>0&&void 0!==e)),i=this.matchIndexes[n]
;return t.splice(0,n),Object.assign(t,i)}}class s{constructor(){
this.rules=[],this.multiRegexes=[],
this.count=0,this.lastIndex=0,this.regexIndex=0}getMatcher(e){
if(this.multiRegexes[e])return this.multiRegexes[e];const t=new i
;return this.rules.slice(e).forEach((([e,n])=>t.addRule(e,n))),
t.compile(),this.multiRegexes[e]=t,t}resumingScanAtSamePosition(){
return 0!==this.regexIndex}considerAll(){this.regexIndex=0}addRule(e,t){
this.rules.push([e,t]),"begin"===t.type&&this.count++}exec(e){
const t=this.getMatcher(this.regexIndex);t.lastIndex=this.lastIndex
;let n=t.exec(e)
;if(this.resumingScanAtSamePosition())if(n&&n.index===this.lastIndex);else{
const t=this.getMatcher(0);t.lastIndex=this.lastIndex+1,n=t.exec(e)}
return n&&(this.regexIndex+=n.position+1,
this.regexIndex===this.count&&this.considerAll()),n}}
if(e.compilerExtensions||(e.compilerExtensions=[]),
e.contains&&e.contains.includes("self"))throw Error("ERR: contains `self` is not supported at the top-level of a language.  See documentation.")
;return e.classNameAliases=a(e.classNameAliases||{}),function t(i,r){const l=i
;if(i.isCompiled)return l
;[A].forEach((e=>e(i,r))),e.compilerExtensions.forEach((e=>e(i,r))),
i.__beforeBegin=null,[M,O,L].forEach((e=>e(i,r))),i.isCompiled=!0;let o=null
;if("object"==typeof i.keywords&&(o=i.keywords.$pattern,
delete i.keywords.$pattern),
i.keywords&&(i.keywords=j(i.keywords,e.case_insensitive)),
i.lexemes&&o)throw Error("ERR: Prefer `keywords.$pattern` to `mode.lexemes`, BOTH are not allowed. (see mode reference) ")
;return o=o||i.lexemes||/\w+/,
l.keywordPatternRe=n(o,!0),r&&(i.begin||(i.begin=/\B|\b/),
l.beginRe=n(i.begin),i.endSameAsBegin&&(i.end=i.begin),
i.end||i.endsWithParent||(i.end=/\B|\b/),
i.end&&(l.endRe=n(i.end)),l.terminatorEnd=g(i.end)||"",
i.endsWithParent&&r.terminatorEnd&&(l.terminatorEnd+=(i.end?"|":"")+r.terminatorEnd)),
i.illegal&&(l.illegalRe=n(i.illegal)),
i.contains||(i.contains=[]),i.contains=[].concat(...i.contains.map((e=>(e=>(e.variants&&!e.cachedVariants&&(e.cachedVariants=e.variants.map((t=>a(e,{
variants:null},t)))),e.cachedVariants?e.cachedVariants:S(e)?a(e,{
starts:e.starts?a(e.starts):null
}):Object.isFrozen(e)?a(e):e))("self"===e?i:e)))),i.contains.forEach((e=>{t(e,l)
})),i.starts&&t(i.starts,r),l.matcher=(e=>{const t=new s
;return e.contains.forEach((e=>t.addRule(e.begin,{rule:e,type:"begin"
}))),e.terminatorEnd&&t.addRule(e.terminatorEnd,{type:"end"
}),e.illegal&&t.addRule(e.illegal,{type:"illegal"}),t})(l),l}(e)}function S(e){
return!!e&&(e.endsWithParent||S(e.starts))}function P(e){const t={
props:["language","code","autodetect"],data:()=>({detectedLanguage:"",
unknownLanguage:!1}),computed:{className(){
return this.unknownLanguage?"":"hljs "+this.detectedLanguage},highlighted(){
if(!this.autoDetect&&!e.getLanguage(this.language))return console.warn(`The language "${this.language}" you specified could not be found.`),
this.unknownLanguage=!0,s(this.code);let t={}
;return this.autoDetect?(t=e.highlightAuto(this.code),
this.detectedLanguage=t.language):(t=e.highlight(this.language,this.code,this.ignoreIllegals),
this.detectedLanguage=this.language),t.value},autoDetect(){
return!(this.language&&(e=this.autodetect,!e&&""!==e));var e},
ignoreIllegals:()=>!0},render(e){return e("pre",{},[e("code",{
class:this.className,domProps:{innerHTML:this.highlighted}})])}};return{
Component:t,VuePlugin:{install(e){e.component("highlightjs",t)}}}}const D={
"after:highlightElement":({el:e,result:t,text:n})=>{const i=H(e)
;if(!i.length)return;const a=document.createElement("div")
;a.innerHTML=t.value,t.value=((e,t,n)=>{let i=0,a="";const r=[];function l(){
return e.length&&t.length?e[0].offset!==t[0].offset?e[0].offset<t[0].offset?e:t:"start"===t[0].event?e:t:e.length?e:t
}function o(e){a+="<"+C(e)+[].map.call(e.attributes,(function(e){
return" "+e.nodeName+'="'+s(e.value)+'"'})).join("")+">"}function c(e){
a+="</"+C(e)+">"}function g(e){("start"===e.event?o:c)(e.node)}
for(;e.length||t.length;){let t=l()
;if(a+=s(n.substring(i,t[0].offset)),i=t[0].offset,t===e){r.reverse().forEach(c)
;do{g(t.splice(0,1)[0]),t=l()}while(t===e&&t.length&&t[0].offset===i)
;r.reverse().forEach(o)
}else"start"===t[0].event?r.push(t[0].node):r.pop(),g(t.splice(0,1)[0])}
return a+s(n.substr(i))})(i,H(a),n)}};function C(e){
return e.nodeName.toLowerCase()}function H(e){const t=[];return function e(n,i){
for(let s=n.firstChild;s;s=s.nextSibling)3===s.nodeType?i+=s.nodeValue.length:1===s.nodeType&&(t.push({
event:"start",offset:i,node:s}),i=e(s,i),C(s).match(/br|hr|img|input/)||t.push({
event:"stop",offset:i,node:s}));return i}(e,0),t}const $={},U=e=>{
console.error(e)},z=(e,...t)=>{console.log("WARN: "+e,...t)},K=(e,t)=>{
$[`${e}/${t}`]||(console.log(`Deprecated as of ${e}. ${t}`),$[`${e}/${t}`]=!0)
},G=s,V=a,W=Symbol("nomatch");return(e=>{
const n=Object.create(null),s=Object.create(null),a=[];let r=!0
;const l=/(^(<[^>]+>|\t|)+|\n)/gm,o="Could not find the language '{}', did you forget to load/include a language module?",g={
disableAutodetect:!0,name:"Plain text",contains:[]};let u={
noHighlightRe:/^(no-?highlight)$/i,
languageDetectRe:/\blang(?:uage)?-([\w-]+)\b/i,classPrefix:"hljs-",
tabReplace:null,useBR:!1,languages:null,__emitter:c};function h(e){
return u.noHighlightRe.test(e)}function d(e,t,n,i){let s="",a=""
;"object"==typeof t?(s=e,
n=t.ignoreIllegals,a=t.language,i=void 0):(K("10.7.0","highlight(lang, code, ...args) has been deprecated."),
K("10.7.0","Please use highlight(code, options) instead.\nhttps://github.com/highlightjs/highlight.js/issues/2277"),
a=e,s=t);const r={code:s,language:a};M("before:highlight",r)
;const l=r.result?r.result:f(r.language,r.code,n,i)
;return l.code=r.code,M("after:highlight",l),l}function f(e,t,s,l){
function c(e,t){const n=v.case_insensitive?t[0].toLowerCase():t[0]
;return Object.prototype.hasOwnProperty.call(e.keywords,n)&&e.keywords[n]}
function g(){null!=R.subLanguage?(()=>{if(""===M)return;let e=null
;if("string"==typeof R.subLanguage){
if(!n[R.subLanguage])return void k.addText(M)
;e=f(R.subLanguage,M,!0,_[R.subLanguage]),_[R.subLanguage]=e.top
}else e=p(M,R.subLanguage.length?R.subLanguage:null)
;R.relevance>0&&(O+=e.relevance),k.addSublanguage(e.emitter,e.language)
})():(()=>{if(!R.keywords)return void k.addText(M);let e=0
;R.keywordPatternRe.lastIndex=0;let t=R.keywordPatternRe.exec(M),n="";for(;t;){
n+=M.substring(e,t.index);const i=c(R,t);if(i){const[e,s]=i
;if(k.addText(n),n="",O+=s,e.startsWith("_"))n+=t[0];else{
const n=v.classNameAliases[e]||e;k.addKeyword(t[0],n)}}else n+=t[0]
;e=R.keywordPatternRe.lastIndex,t=R.keywordPatternRe.exec(M)}
n+=M.substr(e),k.addText(n)})(),M=""}function h(e){
return e.className&&k.openNode(v.classNameAliases[e.className]||e.className),
R=Object.create(e,{parent:{value:R}}),R}function d(e,t,n){let s=((e,t)=>{
const n=e&&e.exec(t);return n&&0===n.index})(e.endRe,n);if(s){if(e["on:end"]){
const n=new i(e);e["on:end"](t,n),n.isMatchIgnored&&(s=!1)}if(s){
for(;e.endsParent&&e.parent;)e=e.parent;return e}}
if(e.endsWithParent)return d(e.parent,t,n)}function m(e){
return 0===R.matcher.regexIndex?(M+=e[0],1):(I=!0,0)}function b(e){
const n=e[0],i=t.substr(e.index),s=d(R,e,i);if(!s)return W;const a=R
;a.skip?M+=n:(a.returnEnd||a.excludeEnd||(M+=n),g(),a.excludeEnd&&(M=n));do{
R.className&&k.closeNode(),R.skip||R.subLanguage||(O+=R.relevance),R=R.parent
}while(R!==s.parent)
;return s.starts&&(s.endSameAsBegin&&(s.starts.endRe=s.endRe),
h(s.starts)),a.returnEnd?0:n.length}let E={};function x(n,a){const l=a&&a[0]
;if(M+=n,null==l)return g(),0
;if("begin"===E.type&&"end"===a.type&&E.index===a.index&&""===l){
if(M+=t.slice(a.index,a.index+1),!r){const t=Error("0 width match regex")
;throw t.languageName=e,t.badRule=E.rule,t}return 1}
if(E=a,"begin"===a.type)return function(e){
const t=e[0],n=e.rule,s=new i(n),a=[n.__beforeBegin,n["on:begin"]]
;for(const n of a)if(n&&(n(e,s),s.isMatchIgnored))return m(t)
;return n&&n.endSameAsBegin&&(n.endRe=RegExp(t.replace(/[-/\\^$*+?.()|[\]{}]/g,"\\$&"),"m")),
n.skip?M+=t:(n.excludeBegin&&(M+=t),
g(),n.returnBegin||n.excludeBegin||(M=t)),h(n),n.returnBegin?0:t.length}(a)
;if("illegal"===a.type&&!s){
const e=Error('Illegal lexeme "'+l+'" for mode "'+(R.className||"<unnamed>")+'"')
;throw e.mode=R,e}if("end"===a.type){const e=b(a);if(e!==W)return e}
if("illegal"===a.type&&""===l)return 1
;if(L>1e5&&L>3*a.index)throw Error("potential infinite loop, way more iterations than matches")
;return M+=l,l.length}const v=N(e)
;if(!v)throw U(o.replace("{}",e)),Error('Unknown language: "'+e+'"')
;const w=T(v,{plugins:a});let y="",R=l||w;const _={},k=new u.__emitter(u);(()=>{
const e=[];for(let t=R;t!==v;t=t.parent)t.className&&e.unshift(t.className)
;e.forEach((e=>k.openNode(e)))})();let M="",O=0,A=0,L=0,I=!1;try{
for(R.matcher.considerAll();;){
L++,I?I=!1:R.matcher.considerAll(),R.matcher.lastIndex=A
;const e=R.matcher.exec(t);if(!e)break;const n=x(t.substring(A,e.index),e)
;A=e.index+n}return x(t.substr(A)),k.closeAllNodes(),k.finalize(),y=k.toHTML(),{
relevance:Math.floor(O),value:y,language:e,illegal:!1,emitter:k,top:R}}catch(n){
if(n.message&&n.message.includes("Illegal"))return{illegal:!0,illegalBy:{
msg:n.message,context:t.slice(A-100,A+100),mode:n.mode},sofar:y,relevance:0,
value:G(t),emitter:k};if(r)return{illegal:!1,relevance:0,value:G(t),emitter:k,
language:e,top:R,errorRaised:n};throw n}}function p(e,t){
t=t||u.languages||Object.keys(n);const i=(e=>{const t={relevance:0,
emitter:new u.__emitter(u),value:G(e),illegal:!1,top:g}
;return t.emitter.addText(e),t})(e),s=t.filter(N).filter(k).map((t=>f(t,e,!1)))
;s.unshift(i);const a=s.sort(((e,t)=>{
if(e.relevance!==t.relevance)return t.relevance-e.relevance
;if(e.language&&t.language){if(N(e.language).supersetOf===t.language)return 1
;if(N(t.language).supersetOf===e.language)return-1}return 0})),[r,l]=a,o=r
;return o.second_best=l,o}const m={"before:highlightElement":({el:e})=>{
u.useBR&&(e.innerHTML=e.innerHTML.replace(/\n/g,"").replace(/<br[ /]*>/g,"\n"))
},"after:highlightElement":({result:e})=>{
u.useBR&&(e.value=e.value.replace(/\n/g,"<br>"))}},b=/^(<[^>]+>|\t)+/gm,E={
"after:highlightElement":({result:e})=>{
u.tabReplace&&(e.value=e.value.replace(b,(e=>e.replace(/\t/g,u.tabReplace))))}}
;function x(e){let t=null;const n=(e=>{let t=e.className+" "
;t+=e.parentNode?e.parentNode.className:"";const n=u.languageDetectRe.exec(t)
;if(n){const t=N(n[1])
;return t||(z(o.replace("{}",n[1])),z("Falling back to no-highlight mode for this block.",e)),
t?n[1]:"no-highlight"}return t.split(/\s+/).find((e=>h(e)||N(e)))})(e)
;if(h(n))return;M("before:highlightElement",{el:e,language:n}),t=e
;const i=t.textContent,a=n?d(i,{language:n,ignoreIllegals:!0}):p(i)
;M("after:highlightElement",{el:e,result:a,text:i
}),e.innerHTML=a.value,((e,t,n)=>{const i=t?s[t]:n
;e.classList.add("hljs"),i&&e.classList.add(i)})(e,n,a.language),e.result={
language:a.language,re:a.relevance,relavance:a.relevance
},a.second_best&&(e.second_best={language:a.second_best.language,
re:a.second_best.relevance,relavance:a.second_best.relevance})}const v=()=>{
v.called||(v.called=!0,
K("10.6.0","initHighlighting() is deprecated.  Use highlightAll() instead."),
document.querySelectorAll("pre code").forEach(x))};let w=!1;function y(){
"loading"!==document.readyState?document.querySelectorAll("pre code").forEach(x):w=!0
}function N(e){return e=(e||"").toLowerCase(),n[e]||n[s[e]]}
function R(e,{languageName:t}){"string"==typeof e&&(e=[e]),e.forEach((e=>{
s[e.toLowerCase()]=t}))}function k(e){const t=N(e)
;return t&&!t.disableAutodetect}function M(e,t){const n=e;a.forEach((e=>{
e[n]&&e[n](t)}))}
"undefined"!=typeof window&&window.addEventListener&&window.addEventListener("DOMContentLoaded",(()=>{
w&&y()}),!1),Object.assign(e,{highlight:d,highlightAuto:p,highlightAll:y,
fixMarkup:e=>{
return K("10.2.0","fixMarkup will be removed entirely in v11.0"),K("10.2.0","Please see https://github.com/highlightjs/highlight.js/issues/2534"),
t=e,
u.tabReplace||u.useBR?t.replace(l,(e=>"\n"===e?u.useBR?"<br>":e:u.tabReplace?e.replace(/\t/g,u.tabReplace):e)):t
;var t},highlightElement:x,
highlightBlock:e=>(K("10.7.0","highlightBlock will be removed entirely in v12.0"),
K("10.7.0","Please use highlightElement now."),x(e)),configure:e=>{
e.useBR&&(K("10.3.0","'useBR' will be removed entirely in v11.0"),
K("10.3.0","Please see https://github.com/highlightjs/highlight.js/issues/2559")),
u=V(u,e)},initHighlighting:v,initHighlightingOnLoad:()=>{
K("10.6.0","initHighlightingOnLoad() is deprecated.  Use highlightAll() instead."),
w=!0},registerLanguage:(t,i)=>{let s=null;try{s=i(e)}catch(e){
if(U("Language definition for '{}' could not be registered.".replace("{}",t)),
!r)throw e;U(e),s=g}
s.name||(s.name=t),n[t]=s,s.rawDefinition=i.bind(null,e),s.aliases&&R(s.aliases,{
languageName:t})},unregisterLanguage:e=>{delete n[e]
;for(const t of Object.keys(s))s[t]===e&&delete s[t]},
listLanguages:()=>Object.keys(n),getLanguage:N,registerAliases:R,
requireLanguage:e=>{
K("10.4.0","requireLanguage will be removed entirely in v11."),
K("10.4.0","Please see https://github.com/highlightjs/highlight.js/pull/2844")
;const t=N(e);if(t)return t
;throw Error("The '{}' language is required, but not loaded.".replace("{}",e))},
autoDetection:k,inherit:V,addPlugin:e=>{(e=>{
e["before:highlightBlock"]&&!e["before:highlightElement"]&&(e["before:highlightElement"]=t=>{
e["before:highlightBlock"](Object.assign({block:t.el},t))
}),e["after:highlightBlock"]&&!e["after:highlightElement"]&&(e["after:highlightElement"]=t=>{
e["after:highlightBlock"](Object.assign({block:t.el},t))})})(e),a.push(e)},
vuePlugin:P(e).VuePlugin}),e.debugMode=()=>{r=!1},e.safeMode=()=>{r=!0
},e.versionString="10.7.2";for(const e in _)"object"==typeof _[e]&&t(_[e])
;return Object.assign(e,_),e.addPlugin(m),e.addPlugin(D),e.addPlugin(E),e})({})
}();"object"==typeof exports&&"undefined"!=typeof module&&(module.exports=hljs);hljs.registerLanguage("python",(()=>{"use strict";return e=>{const n={
$pattern:/[A-Za-z]\w+|__\w+__/,
keyword:["and","as","assert","async","await","break","class","continue","def","del","elif","else","except","finally","for","from","global","if","import","in","is","lambda","nonlocal|10","not","or","pass","raise","return","try","while","with","yield"],
built_in:["__import__","abs","all","any","ascii","bin","bool","breakpoint","bytearray","bytes","callable","chr","classmethod","compile","complex","delattr","dict","dir","divmod","enumerate","eval","exec","filter","float","format","frozenset","getattr","globals","hasattr","hash","help","hex","id","input","int","isinstance","issubclass","iter","len","list","locals","map","max","memoryview","min","next","object","oct","open","ord","pow","print","property","range","repr","reversed","round","set","setattr","slice","sorted","staticmethod","str","sum","super","tuple","type","vars","zip"],
literal:["__debug__","Ellipsis","False","None","NotImplemented","True"],
type:["Any","Callable","Coroutine","Dict","List","Literal","Generic","Optional","Sequence","Set","Tuple","Type","Union"]
},a={className:"meta",begin:/^(>>>|\.\.\.) /},i={className:"subst",begin:/\{/,
end:/\}/,keywords:n,illegal:/#/},s={begin:/\{\{/,relevance:0},t={
className:"string",contains:[e.BACKSLASH_ESCAPE],variants:[{
begin:/([uU]|[bB]|[rR]|[bB][rR]|[rR][bB])?'''/,end:/'''/,
contains:[e.BACKSLASH_ESCAPE,a],relevance:10},{
begin:/([uU]|[bB]|[rR]|[bB][rR]|[rR][bB])?"""/,end:/"""/,
contains:[e.BACKSLASH_ESCAPE,a],relevance:10},{
begin:/([fF][rR]|[rR][fF]|[fF])'''/,end:/'''/,
contains:[e.BACKSLASH_ESCAPE,a,s,i]},{begin:/([fF][rR]|[rR][fF]|[fF])"""/,
end:/"""/,contains:[e.BACKSLASH_ESCAPE,a,s,i]},{begin:/([uU]|[rR])'/,end:/'/,
relevance:10},{begin:/([uU]|[rR])"/,end:/"/,relevance:10},{
begin:/([bB]|[bB][rR]|[rR][bB])'/,end:/'/},{begin:/([bB]|[bB][rR]|[rR][bB])"/,
end:/"/},{begin:/([fF][rR]|[rR][fF]|[fF])'/,end:/'/,
contains:[e.BACKSLASH_ESCAPE,s,i]},{begin:/([fF][rR]|[rR][fF]|[fF])"/,end:/"/,
contains:[e.BACKSLASH_ESCAPE,s,i]},e.APOS_STRING_MODE,e.QUOTE_STRING_MODE]
},r="[0-9](_?[0-9])*",l=`(\\b(${r}))?\\.(${r})|\\b(${r})\\.`,b={
className:"number",relevance:0,variants:[{
begin:`(\\b(${r})|(${l}))[eE][+-]?(${r})[jJ]?\\b`},{begin:`(${l})[jJ]?`},{
begin:"\\b([1-9](_?[0-9])*|0+(_?0)*)[lLjJ]?\\b"},{
begin:"\\b0[bB](_?[01])+[lL]?\\b"},{begin:"\\b0[oO](_?[0-7])+[lL]?\\b"},{
begin:"\\b0[xX](_?[0-9a-fA-F])+[lL]?\\b"},{begin:`\\b(${r})[jJ]\\b`}]},o={
className:"comment",
begin:(d=/# type:/,((...e)=>e.map((e=>(e=>e?"string"==typeof e?e:e.source:null)(e))).join(""))("(?=",d,")")),
end:/$/,keywords:n,contains:[{begin:/# type:/},{begin:/#/,end:/\b\B/,
endsWithParent:!0}]},c={className:"params",variants:[{className:"",
begin:/\(\s*\)/,skip:!0},{begin:/\(/,end:/\)/,excludeBegin:!0,excludeEnd:!0,
keywords:n,contains:["self",a,b,t,e.HASH_COMMENT_MODE]}]};var d
;return i.contains=[t,b,a],{name:"Python",aliases:["py","gyp","ipython"],
keywords:n,illegal:/(<\/|->|\?)|=>/,contains:[a,b,{begin:/\bself\b/},{
beginKeywords:"if",relevance:0},t,o,e.HASH_COMMENT_MODE,{variants:[{
className:"function",beginKeywords:"def"},{className:"class",
beginKeywords:"class"}],end:/:/,illegal:/[${=;\n,]/,
contains:[e.UNDERSCORE_TITLE_MODE,c,{begin:/->/,endsWithParent:!0,keywords:n}]
},{className:"meta",begin:/^[\t ]*@/,end:/(?=#)|$/,contains:[b,c,t]}]}}})());hljs.registerLanguage("reasonml",(()=>{"use strict";return e=>{
const n="~?[a-z$_][0-9a-zA-Z$_]*",a="`?[A-Z$_][0-9a-zA-Z$_]*",s="("+["||","++","**","+.","*","/","*.","/.","..."].map((e=>e.split("").map((e=>"\\"+e)).join(""))).join("|")+"|\\|>|&&|==|===)",i="\\s+"+s+"\\s+",r={
keyword:"and as asr assert begin class constraint do done downto else end exception external for fun function functor if in include inherit initializer land lazy let lor lsl lsr lxor match method mod module mutable new nonrec object of open or private rec sig struct then to try type val virtual when while with",
built_in:"array bool bytes char exn|5 float int int32 int64 list lazy_t|5 nativeint|5 ref string unit ",
literal:"true false"
},l="\\b(0[xX][a-fA-F0-9_]+[Lln]?|0[oO][0-7_]+[Lln]?|0[bB][01_]+[Lln]?|[0-9][0-9_]*([Lln]|(\\.[0-9_]*)?([eE][-+]?[0-9_]+)?)?)",t={
className:"number",relevance:0,variants:[{begin:l},{begin:"\\(-"+l+"\\)"}]},c={
className:"operator",relevance:0,begin:s},o=[{className:"identifier",
relevance:0,begin:n},c,t],g=[e.QUOTE_STRING_MODE,c,{className:"module",
begin:"\\b"+a,returnBegin:!0,end:".",contains:[{className:"identifier",begin:a,
relevance:0}]}],b=[{className:"module",begin:"\\b"+a,returnBegin:!0,end:".",
relevance:0,contains:[{className:"identifier",begin:a,relevance:0}]}],m={
className:"function",relevance:0,keywords:r,variants:[{
begin:"\\s(\\(\\.?.*?\\)|"+n+")\\s*=>",end:"\\s*=>",returnBegin:!0,relevance:0,
contains:[{className:"params",variants:[{begin:n},{
begin:"~?[a-z$_][0-9a-zA-Z$_]*(\\s*:\\s*[a-z$_][0-9a-z$_]*(\\(\\s*('?[a-z$_][0-9a-z$_]*\\s*(,'?[a-z$_][0-9a-z$_]*\\s*)*)?\\))?){0,2}"
},{begin:/\(\s*\)/}]}]},{begin:"\\s\\(\\.?[^;\\|]*\\)\\s*=>",end:"\\s=>",
returnBegin:!0,relevance:0,contains:[{className:"params",relevance:0,variants:[{
begin:n,end:"(,|\\n|\\))",relevance:0,contains:[c,{className:"typing",begin:":",
end:"(,|\\n)",returnBegin:!0,relevance:0,contains:b}]}]}]},{
begin:"\\(\\.\\s"+n+"\\)\\s*=>"}]};g.push(m);const d={className:"constructor",
begin:a+"\\(",end:"\\)",illegal:"\\n",keywords:r,
contains:[e.QUOTE_STRING_MODE,c,{className:"params",begin:"\\b"+n}]},u={
className:"pattern-match",begin:"\\|",returnBegin:!0,keywords:r,end:"=>",
relevance:0,contains:[d,c,{relevance:0,className:"constructor",begin:a}]},v={
className:"module-access",keywords:r,returnBegin:!0,variants:[{
begin:"\\b("+a+"\\.)+"+n},{begin:"\\b("+a+"\\.)+\\(",end:"\\)",returnBegin:!0,
contains:[m,{begin:"\\(",end:"\\)",skip:!0}].concat(g)},{
begin:"\\b("+a+"\\.)+\\{",end:/\}/}],contains:g};return b.push(v),{
name:"ReasonML",aliases:["re"],keywords:r,illegal:"(:-|:=|\\$\\{|\\+=)",
contains:[e.COMMENT("/\\*","\\*/",{illegal:"^(#,\\/\\/)"}),{
className:"character",begin:"'(\\\\[^']+|[^'])'",illegal:"\\n",relevance:0
},e.QUOTE_STRING_MODE,{className:"literal",begin:"\\(\\)",relevance:0},{
className:"literal",begin:"\\[\\|",end:"\\|\\]",relevance:0,contains:o},{
className:"literal",begin:"\\[",end:"\\]",relevance:0,contains:o},d,{
className:"operator",begin:i,illegal:"--\x3e",relevance:0
},t,e.C_LINE_COMMENT_MODE,u,m,{className:"module-def",
begin:"\\bmodule\\s+"+n+"\\s+"+a+"\\s+=\\s+\\{",end:/\}/,returnBegin:!0,
keywords:r,relevance:0,contains:[{className:"module",relevance:0,begin:a},{
begin:/\{/,end:/\}/,skip:!0}].concat(g)},v]}}})());hljs.registerLanguage("capnproto",(()=>{"use strict";return n=>({
name:"Cap\u2019n Proto",aliases:["capnp"],keywords:{
keyword:"struct enum interface union group import using const annotation extends in of on as with from fixed",
built_in:"Void Bool Int8 Int16 Int32 Int64 UInt8 UInt16 UInt32 UInt64 Float32 Float64 Text Data AnyPointer AnyStruct Capability List",
literal:"true false"},
contains:[n.QUOTE_STRING_MODE,n.NUMBER_MODE,n.HASH_COMMENT_MODE,{
className:"meta",begin:/@0x[\w\d]{16};/,illegal:/\n/},{className:"symbol",
begin:/@\d+\b/},{className:"class",beginKeywords:"struct enum",end:/\{/,
illegal:/\n/,contains:[n.inherit(n.TITLE_MODE,{starts:{endsWithParent:!0,
excludeEnd:!0}})]},{className:"class",beginKeywords:"interface",end:/\{/,
illegal:/\n/,contains:[n.inherit(n.TITLE_MODE,{starts:{endsWithParent:!0,
excludeEnd:!0}})]}]})})());hljs.registerLanguage("xml",(()=>{"use strict";function e(e){
return e?"string"==typeof e?e:e.source:null}function n(e){return a("(?=",e,")")}
function a(...n){return n.map((n=>e(n))).join("")}function s(...n){
return"("+n.map((n=>e(n))).join("|")+")"}return e=>{
const t=a(/[A-Z_]/,a("(",/[A-Z0-9_.-]*:/,")?"),/[A-Z0-9_.-]*/),i={
className:"symbol",begin:/&[a-z]+;|&#[0-9]+;|&#x[a-f0-9]+;/},r={begin:/\s/,
contains:[{className:"meta-keyword",begin:/#?[a-z_][a-z1-9_-]+/,illegal:/\n/}]
},c=e.inherit(r,{begin:/\(/,end:/\)/}),l=e.inherit(e.APOS_STRING_MODE,{
className:"meta-string"}),g=e.inherit(e.QUOTE_STRING_MODE,{
className:"meta-string"}),m={endsWithParent:!0,illegal:/</,relevance:0,
contains:[{className:"attr",begin:/[A-Za-z0-9._:-]+/,relevance:0},{begin:/=\s*/,
relevance:0,contains:[{className:"string",endsParent:!0,variants:[{begin:/"/,
end:/"/,contains:[i]},{begin:/'/,end:/'/,contains:[i]},{begin:/[^\s"'=<>`]+/}]}]
}]};return{name:"HTML, XML",
aliases:["html","xhtml","rss","atom","xjb","xsd","xsl","plist","wsf","svg"],
case_insensitive:!0,contains:[{className:"meta",begin:/<![a-z]/,end:/>/,
relevance:10,contains:[r,g,l,c,{begin:/\[/,end:/\]/,contains:[{className:"meta",
begin:/<![a-z]/,end:/>/,contains:[r,c,g,l]}]}]},e.COMMENT(/<!--/,/-->/,{
relevance:10}),{begin:/<!\[CDATA\[/,end:/\]\]>/,relevance:10},i,{
className:"meta",begin:/<\?xml/,end:/\?>/,relevance:10},{className:"tag",
begin:/<style(?=\s|>)/,end:/>/,keywords:{name:"style"},contains:[m],starts:{
end:/<\/style>/,returnEnd:!0,subLanguage:["css","xml"]}},{className:"tag",
begin:/<script(?=\s|>)/,end:/>/,keywords:{name:"script"},contains:[m],starts:{
end:/<\/script>/,returnEnd:!0,subLanguage:["javascript","handlebars","xml"]}},{
className:"tag",begin:/<>|<\/>/},{className:"tag",
begin:a(/</,n(a(t,s(/\/>/,/>/,/\s/)))),end:/\/?>/,contains:[{className:"name",
begin:t,relevance:0,starts:m}]},{className:"tag",begin:a(/<\//,n(a(t,/>/))),
contains:[{className:"name",begin:t,relevance:0},{begin:/>/,relevance:0,
endsParent:!0}]}]}}})());hljs.registerLanguage("markdown",(()=>{"use strict";function n(...n){
return n.map((n=>{return(e=n)?"string"==typeof e?e:e.source:null;var e
})).join("")}return e=>{const a={begin:/<\/?[A-Za-z_]/,end:">",
subLanguage:"xml",relevance:0},i={variants:[{begin:/\[.+?\]\[.*?\]/,relevance:0
},{begin:/\[.+?\]\(((data|javascript|mailto):|(?:http|ftp)s?:\/\/).*?\)/,
relevance:2},{begin:n(/\[.+?\]\(/,/[A-Za-z][A-Za-z0-9+.-]*/,/:\/\/.*?\)/),
relevance:2},{begin:/\[.+?\]\([./?&#].*?\)/,relevance:1},{
begin:/\[.+?\]\(.*?\)/,relevance:0}],returnBegin:!0,contains:[{
className:"string",relevance:0,begin:"\\[",end:"\\]",excludeBegin:!0,
returnEnd:!0},{className:"link",relevance:0,begin:"\\]\\(",end:"\\)",
excludeBegin:!0,excludeEnd:!0},{className:"symbol",relevance:0,begin:"\\]\\[",
end:"\\]",excludeBegin:!0,excludeEnd:!0}]},s={className:"strong",contains:[],
variants:[{begin:/_{2}/,end:/_{2}/},{begin:/\*{2}/,end:/\*{2}/}]},c={
className:"emphasis",contains:[],variants:[{begin:/\*(?!\*)/,end:/\*/},{
begin:/_(?!_)/,end:/_/,relevance:0}]};s.contains.push(c),c.contains.push(s)
;let t=[a,i]
;return s.contains=s.contains.concat(t),c.contains=c.contains.concat(t),
t=t.concat(s,c),{name:"Markdown",aliases:["md","mkdown","mkd"],contains:[{
className:"section",variants:[{begin:"^#{1,6}",end:"$",contains:t},{
begin:"(?=^.+?\\n[=-]{2,}$)",contains:[{begin:"^[=-]*$"},{begin:"^",end:"\\n",
contains:t}]}]},a,{className:"bullet",begin:"^[ \t]*([*+-]|(\\d+\\.))(?=\\s+)",
end:"\\s+",excludeEnd:!0},s,c,{className:"quote",begin:"^>\\s+",contains:t,
end:"$"},{className:"code",variants:[{begin:"(`{3,})[^`](.|\\n)*?\\1`*[ ]*"},{
begin:"(~{3,})[^~](.|\\n)*?\\1~*[ ]*"},{begin:"```",end:"```+[ ]*$"},{
begin:"~~~",end:"~~~+[ ]*$"},{begin:"`.+?`"},{begin:"(?=^( {4}|\\t))",
contains:[{begin:"^( {4}|\\t)",end:"(\\n)$"}],relevance:0}]},{
begin:"^[-\\*]{3,}",end:"$"},i,{begin:/^\[[^\n]+\]:/,returnBegin:!0,contains:[{
className:"symbol",begin:/\[/,end:/\]/,excludeBegin:!0,excludeEnd:!0},{
className:"link",begin:/:\s*/,end:/$/,excludeBegin:!0}]}]}}})());hljs.registerLanguage("diff",(()=>{"use strict";return e=>({name:"Diff",
aliases:["patch"],contains:[{className:"meta",relevance:10,variants:[{
begin:/^@@ +-\d+,\d+ +\+\d+,\d+ +@@/},{begin:/^\*\*\* +\d+,\d+ +\*\*\*\*$/},{
begin:/^--- +\d+,\d+ +----$/}]},{className:"comment",variants:[{begin:/Index: /,
end:/$/},{begin:/^index/,end:/$/},{begin:/={3,}/,end:/$/},{begin:/^-{3}/,end:/$/
},{begin:/^\*{3} /,end:/$/},{begin:/^\+{3}/,end:/$/},{begin:/^\*{15}$/},{
begin:/^diff --git/,end:/$/}]},{className:"addition",begin:/^\+/,end:/$/},{
className:"deletion",begin:/^-/,end:/$/},{className:"addition",begin:/^!/,
end:/$/}]})})());hljs.registerLanguage("bash",(()=>{"use strict";function e(...e){
return e.map((e=>{return(s=e)?"string"==typeof s?s:s.source:null;var s
})).join("")}return s=>{const n={},t={begin:/\$\{/,end:/\}/,contains:["self",{
begin:/:-/,contains:[n]}]};Object.assign(n,{className:"variable",variants:[{
begin:e(/\$[\w\d#@][\w\d_]*/,"(?![\\w\\d])(?![$])")},t]});const a={
className:"subst",begin:/\$\(/,end:/\)/,contains:[s.BACKSLASH_ESCAPE]},i={
begin:/<<-?\s*(?=\w+)/,starts:{contains:[s.END_SAME_AS_BEGIN({begin:/(\w+)/,
end:/(\w+)/,className:"string"})]}},c={className:"string",begin:/"/,end:/"/,
contains:[s.BACKSLASH_ESCAPE,n,a]};a.contains.push(c);const o={begin:/\$\(\(/,
end:/\)\)/,contains:[{begin:/\d+#[0-9a-f]+/,className:"number"},s.NUMBER_MODE,n]
},r=s.SHEBANG({binary:"(fish|bash|zsh|sh|csh|ksh|tcsh|dash|scsh)",relevance:10
}),l={className:"function",begin:/\w[\w\d_]*\s*\(\s*\)\s*\{/,returnBegin:!0,
contains:[s.inherit(s.TITLE_MODE,{begin:/\w[\w\d_]*/})],relevance:0};return{
name:"Bash",aliases:["sh","zsh"],keywords:{$pattern:/\b[a-z._-]+\b/,
keyword:"if then else elif fi for while in do done case esac function",
literal:"true false",
built_in:"break cd continue eval exec exit export getopts hash pwd readonly return shift test times trap umask unset alias bind builtin caller command declare echo enable help let local logout mapfile printf read readarray source type typeset ulimit unalias set shopt autoload bg bindkey bye cap chdir clone comparguments compcall compctl compdescribe compfiles compgroups compquote comptags comptry compvalues dirs disable disown echotc echoti emulate fc fg float functions getcap getln history integer jobs kill limit log noglob popd print pushd pushln rehash sched setcap setopt stat suspend ttyctl unfunction unhash unlimit unsetopt vared wait whence where which zcompile zformat zftp zle zmodload zparseopts zprof zpty zregexparse zsocket zstyle ztcp"
},contains:[r,s.SHEBANG(),l,o,s.HASH_COMMENT_MODE,i,c,{className:"",begin:/\\"/
},{className:"string",begin:/'/,end:/'/},n]}}})());hljs.registerLanguage("ocaml",(()=>{"use strict";return e=>({name:"OCaml",
aliases:["ml"],keywords:{$pattern:"[a-z_]\\w*!?",
keyword:"and as assert asr begin class constraint do done downto else end exception external for fun function functor if in include inherit! inherit initializer land lazy let lor lsl lsr lxor match method!|10 method mod module mutable new object of open! open or private rec sig struct then to try type val! val virtual when while with parser value",
built_in:"array bool bytes char exn|5 float int int32 int64 list lazy_t|5 nativeint|5 string unit in_channel out_channel ref",
literal:"true false"},illegal:/\/\/|>>/,contains:[{className:"literal",
begin:"\\[(\\|\\|)?\\]|\\(\\)",relevance:0},e.COMMENT("\\(\\*","\\*\\)",{
contains:["self"]}),{className:"symbol",begin:"'[A-Za-z_](?!')[\\w']*"},{
className:"type",begin:"`[A-Z][\\w']*"},{className:"type",
begin:"\\b[A-Z][\\w']*",relevance:0},{begin:"[a-z_]\\w*'[\\w']*",relevance:0
},e.inherit(e.APOS_STRING_MODE,{className:"string",relevance:0
}),e.inherit(e.QUOTE_STRING_MODE,{illegal:null}),{className:"number",
begin:"\\b(0[xX][a-fA-F0-9_]+[Lln]?|0[oO][0-7_]+[Lln]?|0[bB][01_]+[Lln]?|[0-9][0-9_]*([Lln]|(\\.[0-9_]*)?([eE][-+]?[0-9_]+)?)?)",
relevance:0},{begin:/->/}]})})());hljs.registerLanguage("ruby",(()=>{"use strict";function e(...e){
return e.map((e=>{return(n=e)?"string"==typeof n?n:n.source:null;var n
})).join("")}return n=>{
const a="([a-zA-Z_]\\w*[!?=]?|[-+~]@|<<|>>|=~|===?|<=>|[<>]=?|\\*\\*|[-/+%^&*~`|]|\\[\\]=?)",i={
keyword:"and then defined module in return redo if BEGIN retry end for self when next until do begin unless END rescue else break undef not super class case require yield alias while ensure elsif or include attr_reader attr_writer attr_accessor __FILE__",
built_in:"proc lambda",literal:"true false nil"},s={className:"doctag",
begin:"@[A-Za-z]+"},r={begin:"#<",end:">"},b=[n.COMMENT("#","$",{contains:[s]
}),n.COMMENT("^=begin","^=end",{contains:[s],relevance:10
}),n.COMMENT("^__END__","\\n$")],c={className:"subst",begin:/#\{/,end:/\}/,
keywords:i},t={className:"string",contains:[n.BACKSLASH_ESCAPE,c],variants:[{
begin:/'/,end:/'/},{begin:/"/,end:/"/},{begin:/`/,end:/`/},{begin:/%[qQwWx]?\(/,
end:/\)/},{begin:/%[qQwWx]?\[/,end:/\]/},{begin:/%[qQwWx]?\{/,end:/\}/},{
begin:/%[qQwWx]?</,end:/>/},{begin:/%[qQwWx]?\//,end:/\//},{begin:/%[qQwWx]?%/,
end:/%/},{begin:/%[qQwWx]?-/,end:/-/},{begin:/%[qQwWx]?\|/,end:/\|/},{
begin:/\B\?(\\\d{1,3})/},{begin:/\B\?(\\x[A-Fa-f0-9]{1,2})/},{
begin:/\B\?(\\u\{?[A-Fa-f0-9]{1,6}\}?)/},{
begin:/\B\?(\\M-\\C-|\\M-\\c|\\c\\M-|\\M-|\\C-\\M-)[\x20-\x7e]/},{
begin:/\B\?\\(c|C-)[\x20-\x7e]/},{begin:/\B\?\\?\S/},{
begin:/<<[-~]?'?(\w+)\n(?:[^\n]*\n)*?\s*\1\b/,returnBegin:!0,contains:[{
begin:/<<[-~]?'?/},n.END_SAME_AS_BEGIN({begin:/(\w+)/,end:/(\w+)/,
contains:[n.BACKSLASH_ESCAPE,c]})]}]},g="[0-9](_?[0-9])*",d={className:"number",
relevance:0,variants:[{
begin:`\\b([1-9](_?[0-9])*|0)(\\.(${g}))?([eE][+-]?(${g})|r)?i?\\b`},{
begin:"\\b0[dD][0-9](_?[0-9])*r?i?\\b"},{begin:"\\b0[bB][0-1](_?[0-1])*r?i?\\b"
},{begin:"\\b0[oO][0-7](_?[0-7])*r?i?\\b"},{
begin:"\\b0[xX][0-9a-fA-F](_?[0-9a-fA-F])*r?i?\\b"},{
begin:"\\b0(_?[0-7])+r?i?\\b"}]},l={className:"params",begin:"\\(",end:"\\)",
endsParent:!0,keywords:i},o=[t,{className:"class",beginKeywords:"class module",
end:"$|;",illegal:/=/,contains:[n.inherit(n.TITLE_MODE,{
begin:"[A-Za-z_]\\w*(::\\w+)*(\\?|!)?"}),{begin:"<\\s*",contains:[{
begin:"("+n.IDENT_RE+"::)?"+n.IDENT_RE,relevance:0}]}].concat(b)},{
className:"function",begin:e(/def\s+/,(_=a+"\\s*(\\(|;|$)",e("(?=",_,")"))),
relevance:0,keywords:"def",end:"$|;",contains:[n.inherit(n.TITLE_MODE,{begin:a
}),l].concat(b)},{begin:n.IDENT_RE+"::"},{className:"symbol",
begin:n.UNDERSCORE_IDENT_RE+"(!|\\?)?:",relevance:0},{className:"symbol",
begin:":(?!\\s)",contains:[t,{begin:a}],relevance:0},d,{className:"variable",
begin:"(\\$\\W)|((\\$|@@?)(\\w+))(?=[^@$?])(?![A-Za-z])(?![@$?'])"},{
className:"params",begin:/\|/,end:/\|/,relevance:0,keywords:i},{
begin:"("+n.RE_STARTERS_RE+"|unless)\\s*",keywords:"unless",contains:[{
className:"regexp",contains:[n.BACKSLASH_ESCAPE,c],illegal:/\n/,variants:[{
begin:"/",end:"/[a-z]*"},{begin:/%r\{/,end:/\}[a-z]*/},{begin:"%r\\(",
end:"\\)[a-z]*"},{begin:"%r!",end:"![a-z]*"},{begin:"%r\\[",end:"\\][a-z]*"}]
}].concat(r,b),relevance:0}].concat(r,b);var _;c.contains=o,l.contains=o
;const E=[{begin:/^\s*=>/,starts:{end:"$",contains:o}},{className:"meta",
begin:"^([>?]>|[\\w#]+\\(\\w+\\):\\d+:\\d+>|(\\w+-)?\\d+\\.\\d+\\.\\d+(p\\d+)?[^\\d][^>]+>)(?=[ ])",
starts:{end:"$",contains:o}}];return b.unshift(r),{name:"Ruby",
aliases:["rb","gemspec","podspec","thor","irb"],keywords:i,illegal:/\/\*/,
contains:[n.SHEBANG({binary:"ruby"})].concat(E).concat(b).concat(o)}}})());hljs.registerLanguage("yaml",(()=>{"use strict";return e=>{
var n="true false yes no null",a="[\\w#;/?:@&=+$,.~*'()[\\]]+",s={
className:"string",relevance:0,variants:[{begin:/'/,end:/'/},{begin:/"/,end:/"/
},{begin:/\S+/}],contains:[e.BACKSLASH_ESCAPE,{className:"template-variable",
variants:[{begin:/\{\{/,end:/\}\}/},{begin:/%\{/,end:/\}/}]}]},i=e.inherit(s,{
variants:[{begin:/'/,end:/'/},{begin:/"/,end:/"/},{begin:/[^\s,{}[\]]+/}]}),l={
end:",",endsWithParent:!0,excludeEnd:!0,keywords:n,relevance:0},t={begin:/\{/,
end:/\}/,contains:[l],illegal:"\\n",relevance:0},g={begin:"\\[",end:"\\]",
contains:[l],illegal:"\\n",relevance:0},b=[{className:"attr",variants:[{
begin:"\\w[\\w :\\/.-]*:(?=[ \t]|$)"},{begin:'"\\w[\\w :\\/.-]*":(?=[ \t]|$)'},{
begin:"'\\w[\\w :\\/.-]*':(?=[ \t]|$)"}]},{className:"meta",begin:"^---\\s*$",
relevance:10},{className:"string",
begin:"[\\|>]([1-9]?[+-])?[ ]*\\n( +)[^ ][^\\n]*\\n(\\2[^\\n]+\\n?)*"},{
begin:"<%[%=-]?",end:"[%-]?%>",subLanguage:"ruby",excludeBegin:!0,excludeEnd:!0,
relevance:0},{className:"type",begin:"!\\w+!"+a},{className:"type",
begin:"!<"+a+">"},{className:"type",begin:"!"+a},{className:"type",begin:"!!"+a
},{className:"meta",begin:"&"+e.UNDERSCORE_IDENT_RE+"$"},{className:"meta",
begin:"\\*"+e.UNDERSCORE_IDENT_RE+"$"},{className:"bullet",begin:"-(?=[ ]|$)",
relevance:0},e.HASH_COMMENT_MODE,{beginKeywords:n,keywords:{literal:n}},{
className:"number",
begin:"\\b[0-9]{4}(-[0-9][0-9]){0,2}([Tt \\t][0-9][0-9]?(:[0-9][0-9]){2})?(\\.[0-9]*)?([ \\t])*(Z|[-+][0-9][0-9]?(:[0-9][0-9])?)?\\b"
},{className:"number",begin:e.C_NUMBER_RE+"\\b",relevance:0},t,g,s],r=[...b]
;return r.pop(),r.push(i),l.contains=r,{name:"YAML",case_insensitive:!0,
aliases:["yml"],contains:b}}})());hljs.registerLanguage("json",(()=>{"use strict";return n=>{const e={
literal:"true false null"
},i=[n.C_LINE_COMMENT_MODE,n.C_BLOCK_COMMENT_MODE],a=[n.QUOTE_STRING_MODE,n.C_NUMBER_MODE],l={
end:",",endsWithParent:!0,excludeEnd:!0,contains:a,keywords:e},t={begin:/\{/,
end:/\}/,contains:[{className:"attr",begin:/"/,end:/"/,
contains:[n.BACKSLASH_ESCAPE],illegal:"\\n"},n.inherit(l,{begin:/:/
})].concat(i),illegal:"\\S"},s={begin:"\\[",end:"\\]",contains:[n.inherit(l)],
illegal:"\\S"};return a.push(t,s),i.forEach((n=>{a.push(n)})),{name:"JSON",
contains:a,keywords:e,illegal:"\\S"}}})());hljs.registerLanguage("makefile",(()=>{"use strict";return e=>{const i={
className:"variable",variants:[{begin:"\\$\\("+e.UNDERSCORE_IDENT_RE+"\\)",
contains:[e.BACKSLASH_ESCAPE]},{begin:/\$[@%<?\^\+\*]/}]},a={className:"string",
begin:/"/,end:/"/,contains:[e.BACKSLASH_ESCAPE,i]},n={className:"variable",
begin:/\$\([\w-]+\s/,end:/\)/,keywords:{
built_in:"subst patsubst strip findstring filter filter-out sort word wordlist firstword lastword dir notdir suffix basename addsuffix addprefix join wildcard realpath abspath error warning shell origin flavor foreach if or and call eval file value"
},contains:[i]},s={begin:"^"+e.UNDERSCORE_IDENT_RE+"\\s*(?=[:+?]?=)"},r={
className:"section",begin:/^[^\s]+:/,end:/$/,contains:[i]};return{
name:"Makefile",aliases:["mk","mak","make"],keywords:{$pattern:/[\w-]+/,
keyword:"define endef undefine ifdef ifndef ifeq ifneq else endif include -include sinclude override export unexport private vpath"
},contains:[e.HASH_COMMENT_MODE,i,a,n,s,{className:"meta",begin:/^\.PHONY:/,
end:/$/,keywords:{$pattern:/[\.\w]+/,"meta-keyword":".PHONY"}},r]}}})());hljs.registerLanguage("plaintext",(()=>{"use strict";return t=>({
name:"Plain text",aliases:["text","txt"],disableAutodetect:!0})})());hljs.registerLanguage("css",(()=>{"use strict"
;const e=["a","abbr","address","article","aside","audio","b","blockquote","body","button","canvas","caption","cite","code","dd","del","details","dfn","div","dl","dt","em","fieldset","figcaption","figure","footer","form","h1","h2","h3","h4","h5","h6","header","hgroup","html","i","iframe","img","input","ins","kbd","label","legend","li","main","mark","menu","nav","object","ol","p","q","quote","samp","section","span","strong","summary","sup","table","tbody","td","textarea","tfoot","th","thead","time","tr","ul","var","video"],t=["any-hover","any-pointer","aspect-ratio","color","color-gamut","color-index","device-aspect-ratio","device-height","device-width","display-mode","forced-colors","grid","height","hover","inverted-colors","monochrome","orientation","overflow-block","overflow-inline","pointer","prefers-color-scheme","prefers-contrast","prefers-reduced-motion","prefers-reduced-transparency","resolution","scan","scripting","update","width","min-width","max-width","min-height","max-height"],i=["active","any-link","blank","checked","current","default","defined","dir","disabled","drop","empty","enabled","first","first-child","first-of-type","fullscreen","future","focus","focus-visible","focus-within","has","host","host-context","hover","indeterminate","in-range","invalid","is","lang","last-child","last-of-type","left","link","local-link","not","nth-child","nth-col","nth-last-child","nth-last-col","nth-last-of-type","nth-of-type","only-child","only-of-type","optional","out-of-range","past","placeholder-shown","read-only","read-write","required","right","root","scope","target","target-within","user-invalid","valid","visited","where"],o=["after","backdrop","before","cue","cue-region","first-letter","first-line","grammar-error","marker","part","placeholder","selection","slotted","spelling-error"],r=["align-content","align-items","align-self","animation","animation-delay","animation-direction","animation-duration","animation-fill-mode","animation-iteration-count","animation-name","animation-play-state","animation-timing-function","auto","backface-visibility","background","background-attachment","background-clip","background-color","background-image","background-origin","background-position","background-repeat","background-size","border","border-bottom","border-bottom-color","border-bottom-left-radius","border-bottom-right-radius","border-bottom-style","border-bottom-width","border-collapse","border-color","border-image","border-image-outset","border-image-repeat","border-image-slice","border-image-source","border-image-width","border-left","border-left-color","border-left-style","border-left-width","border-radius","border-right","border-right-color","border-right-style","border-right-width","border-spacing","border-style","border-top","border-top-color","border-top-left-radius","border-top-right-radius","border-top-style","border-top-width","border-width","bottom","box-decoration-break","box-shadow","box-sizing","break-after","break-before","break-inside","caption-side","clear","clip","clip-path","color","column-count","column-fill","column-gap","column-rule","column-rule-color","column-rule-style","column-rule-width","column-span","column-width","columns","content","counter-increment","counter-reset","cursor","direction","display","empty-cells","filter","flex","flex-basis","flex-direction","flex-flow","flex-grow","flex-shrink","flex-wrap","float","font","font-display","font-family","font-feature-settings","font-kerning","font-language-override","font-size","font-size-adjust","font-smoothing","font-stretch","font-style","font-variant","font-variant-ligatures","font-variation-settings","font-weight","height","hyphens","icon","image-orientation","image-rendering","image-resolution","ime-mode","inherit","initial","justify-content","left","letter-spacing","line-height","list-style","list-style-image","list-style-position","list-style-type","margin","margin-bottom","margin-left","margin-right","margin-top","marks","mask","max-height","max-width","min-height","min-width","nav-down","nav-index","nav-left","nav-right","nav-up","none","normal","object-fit","object-position","opacity","order","orphans","outline","outline-color","outline-offset","outline-style","outline-width","overflow","overflow-wrap","overflow-x","overflow-y","padding","padding-bottom","padding-left","padding-right","padding-top","page-break-after","page-break-before","page-break-inside","perspective","perspective-origin","pointer-events","position","quotes","resize","right","src","tab-size","table-layout","text-align","text-align-last","text-decoration","text-decoration-color","text-decoration-line","text-decoration-style","text-indent","text-overflow","text-rendering","text-shadow","text-transform","text-underline-position","top","transform","transform-origin","transform-style","transition","transition-delay","transition-duration","transition-property","transition-timing-function","unicode-bidi","vertical-align","visibility","white-space","widows","width","word-break","word-spacing","word-wrap","z-index"].reverse()
;return n=>{const a=(e=>({IMPORTANT:{className:"meta",begin:"!important"},
HEXCOLOR:{className:"number",begin:"#([a-fA-F0-9]{6}|[a-fA-F0-9]{3})"},
ATTRIBUTE_SELECTOR_MODE:{className:"selector-attr",begin:/\[/,end:/\]/,
illegal:"$",contains:[e.APOS_STRING_MODE,e.QUOTE_STRING_MODE]}
}))(n),l=[n.APOS_STRING_MODE,n.QUOTE_STRING_MODE];return{name:"CSS",
case_insensitive:!0,illegal:/[=|'\$]/,keywords:{keyframePosition:"from to"},
classNameAliases:{keyframePosition:"selector-tag"},
contains:[n.C_BLOCK_COMMENT_MODE,{begin:/-(webkit|moz|ms|o)-(?=[a-z])/
},n.CSS_NUMBER_MODE,{className:"selector-id",begin:/#[A-Za-z0-9_-]+/,relevance:0
},{className:"selector-class",begin:"\\.[a-zA-Z-][a-zA-Z0-9_-]*",relevance:0
},a.ATTRIBUTE_SELECTOR_MODE,{className:"selector-pseudo",variants:[{
begin:":("+i.join("|")+")"},{begin:"::("+o.join("|")+")"}]},{
className:"attribute",begin:"\\b("+r.join("|")+")\\b"},{begin:":",end:"[;}]",
contains:[a.HEXCOLOR,a.IMPORTANT,n.CSS_NUMBER_MODE,...l,{
begin:/(url|data-uri)\(/,end:/\)/,relevance:0,keywords:{built_in:"url data-uri"
},contains:[{className:"string",begin:/[^)]/,endsWithParent:!0,excludeEnd:!0}]
},{className:"built_in",begin:/[\w-]+(?=\()/}]},{
begin:(s=/@/,((...e)=>e.map((e=>(e=>e?"string"==typeof e?e:e.source:null)(e))).join(""))("(?=",s,")")),
end:"[{;]",relevance:0,illegal:/:/,contains:[{className:"keyword",
begin:/@-?\w[\w]*(-\w+)*/},{begin:/\s/,endsWithParent:!0,excludeEnd:!0,
relevance:0,keywords:{$pattern:/[a-z-]+/,keyword:"and or not only",
attribute:t.join(" ")},contains:[{begin:/[a-z-]+(?=:)/,className:"attribute"
},...l,n.CSS_NUMBER_MODE]}]},{className:"selector-tag",
begin:"\\b("+e.join("|")+")\\b"}]};var s}})());hljs.registerLanguage("ini",(()=>{"use strict";function e(e){
return e?"string"==typeof e?e:e.source:null}function n(...n){
return n.map((n=>e(n))).join("")}return s=>{const a={className:"number",
relevance:0,variants:[{begin:/([+-]+)?[\d]+_[\d_]+/},{begin:s.NUMBER_RE}]
},i=s.COMMENT();i.variants=[{begin:/;/,end:/$/},{begin:/#/,end:/$/}];const t={
className:"variable",variants:[{begin:/\$[\w\d"][\w\d_]*/},{begin:/\$\{(.*?)\}/
}]},r={className:"literal",begin:/\bon|off|true|false|yes|no\b/},l={
className:"string",contains:[s.BACKSLASH_ESCAPE],variants:[{begin:"'''",
end:"'''",relevance:10},{begin:'"""',end:'"""',relevance:10},{begin:'"',end:'"'
},{begin:"'",end:"'"}]},c={begin:/\[/,end:/\]/,contains:[i,r,t,l,a,"self"],
relevance:0
},g="("+[/[A-Za-z0-9_-]+/,/"(\\"|[^"])*"/,/'[^']*'/].map((n=>e(n))).join("|")+")"
;return{name:"TOML, also INI",aliases:["toml"],case_insensitive:!0,illegal:/\S/,
contains:[i,{className:"section",begin:/\[+/,end:/\]+/},{
begin:n(g,"(\\s*\\.\\s*",g,")*",n("(?=",/\s*=\s*[^#\s]/,")")),className:"attr",
starts:{end:/$/,contains:[i,c,r,t,l,a]}}]}}})());hljs.registerLanguage("coq",(()=>{"use strict";return e=>({name:"Coq",keywords:{
keyword:"_|0 as at cofix else end exists exists2 fix for forall fun if IF in let match mod Prop return Set then Type using where with Abort About Add Admit Admitted All Arguments Assumptions Axiom Back BackTo Backtrack Bind Blacklist Canonical Cd Check Class Classes Close Coercion Coercions CoFixpoint CoInductive Collection Combined Compute Conjecture Conjectures Constant constr Constraint Constructors Context Corollary CreateHintDb Cut Declare Defined Definition Delimit Dependencies Dependent Derive Drop eauto End Equality Eval Example Existential Existentials Existing Export exporting Extern Extract Extraction Fact Field Fields File Fixpoint Focus for From Function Functional Generalizable Global Goal Grab Grammar Graph Guarded Heap Hint HintDb Hints Hypotheses Hypothesis ident Identity If Immediate Implicit Import Include Inductive Infix Info Initial Inline Inspect Instance Instances Intro Intros Inversion Inversion_clear Language Left Lemma Let Libraries Library Load LoadPath Local Locate Ltac ML Mode Module Modules Monomorphic Morphism Next NoInline Notation Obligation Obligations Opaque Open Optimize Options Parameter Parameters Parametric Path Paths pattern Polymorphic Preterm Print Printing Program Projections Proof Proposition Pwd Qed Quit Rec Record Recursive Redirect Relation Remark Remove Require Reserved Reset Resolve Restart Rewrite Right Ring Rings Save Scheme Scope Scopes Script Search SearchAbout SearchHead SearchPattern SearchRewrite Section Separate Set Setoid Show Solve Sorted Step Strategies Strategy Structure SubClass Table Tables Tactic Term Test Theorem Time Timeout Transparent Type Typeclasses Types Undelimit Undo Unfocus Unfocused Unfold Universe Universes Unset Unshelve using Variable Variables Variant Verbose Visibility where with",
built_in:"abstract absurd admit after apply as assert assumption at auto autorewrite autounfold before bottom btauto by case case_eq cbn cbv change classical_left classical_right clear clearbody cofix compare compute congruence constr_eq constructor contradict contradiction cut cutrewrite cycle decide decompose dependent destruct destruction dintuition discriminate discrR do double dtauto eapply eassumption eauto ecase econstructor edestruct ediscriminate eelim eexact eexists einduction einjection eleft elim elimtype enough equality erewrite eright esimplify_eq esplit evar exact exactly_once exfalso exists f_equal fail field field_simplify field_simplify_eq first firstorder fix fold fourier functional generalize generalizing gfail give_up has_evar hnf idtac in induction injection instantiate intro intro_pattern intros intuition inversion inversion_clear is_evar is_var lapply lazy left lia lra move native_compute nia nsatz omega once pattern pose progress proof psatz quote record red refine reflexivity remember rename repeat replace revert revgoals rewrite rewrite_strat right ring ring_simplify rtauto set setoid_reflexivity setoid_replace setoid_rewrite setoid_symmetry setoid_transitivity shelve shelve_unifiable simpl simple simplify_eq solve specialize split split_Rabs split_Rmult stepl stepr subst sum swap symmetry tactic tauto time timeout top transitivity trivial try tryif unfold unify until using vm_compute with"
},contains:[e.QUOTE_STRING_MODE,e.COMMENT("\\(\\*","\\*\\)"),e.C_NUMBER_MODE,{
className:"type",excludeBegin:!0,begin:"\\|\\s*",end:"\\w+"},{begin:/[-=]>/}]})
})());