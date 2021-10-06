
/* PARSER/INTERPRETER FOR SYSCOMPILER PROJECT 2S 2021 */

/* LEXICAL DEFINITIONS SECTION */
%lex
%options case-insensitive 

//REGULAR EXPRESSIONS SECTION
num         [0-9]+
digit       [0-9]
letter      [a-zA-ZÑñ]+
integer     ({digit})+
doublenum   {digit}+"."{digit}+
id          {letter}({letter}|{digit}|"_")*
//--> STRINGS
escapechar   [\'\"\\ntr]
escape       \\{escapechar}
acceptance  [^\'\\]
string      (\"({escape} | {acceptance})*\")

//--> CHARACTER
escapechar2 [\'\"\\ntr"]
escape2     \\{escapechar2}
acceptance2 [^\'\\]
character   (\'({escape2} | {acceptance2})\')

%%

/* Comentarios */
"//".*              {/*Ignoramos los comentarios simples*/}   id++
"/*"((\*+[^/*])|([^*]))*\**"*/" {/*ignorar comentarios con multiples lineas*/}

/* Simbolos del programa */
"=="                  { console.log("Reconocio : " + yytext);  return 'EQUALTO' }

"."                  { console.log("Reconocio : " + yytext);  return 'DOT' }
","                  { console.log("Reconocio : " + yytext);  return 'COMMA' }
";"                  { console.log("Reconocio : " + yytext);  return 'SEMICOLON' }
":"                  { console.log("Reconocio : " + yytext);  return 'COLON' }
"["                  { console.log("Reconocio : " + yytext);  return 'LSBRACKET' }
"]"                  { console.log("Reconocio : " + yytext);  return 'RSBRACKET' }
"("                  { console.log("Reconocio : " + yytext);  return 'LPAR' }
")"                  { console.log("Reconocio : " + yytext);  return 'RPAR' }
"{"                  { console.log("Reconocio : " + yytext);  return 'LCBRACKET' }
"}"                  { console.log("Reconocio : " + yytext);  return 'RCBRACKET' }
"="                  { console.log("Reconocio : " + yytext);  return 'EQUAL' }

/* MATHEMATICAL OPERATORS */
"+"                  { console.log("Reconocio : " + yytext);  return 'PLUS' } 
"*"                  { console.log("Reconocio : " + yytext);  return 'MULTI' } 
"/"                  { console.log("Reconocio : " + yytext);  return 'DIV' } 
"-"                  { console.log("Reconocio : " + yytext);  return 'MINUS' } 
"%"                  { console.log("Reconocio : " + yytext);  return 'MOD' } 
"^"                  { console.log("Reconocio : " + yytext);  return 'POT' }
"PI"                 { console.log("Reconocio : " + yytext);  return 'PI' } 
"E"                  { console.log("Reconocio : " + yytext);  return 'E' }

/* RELATIONAL OPERATORS */
"<="                  { console.log("Reconocio : " + yytext);  return 'LESSEQUAL' }
"<"                  { console.log("Reconocio : " + yytext);  return 'LESSTHAN' }
">="                  { console.log("Reconocio : " + yytext);  return 'GREATEREQUAL' }
">"                  { console.log("Reconocio : " + yytext);  return 'GREATERTHAN' }
"!="                  { console.log("Reconocio : " + yytext);  return 'NOTEQUAL' }

/* LOGICAL OPERATORS */
"&&"                  { console.log("Reconocio : " + yytext);  return 'AND' }
"||"                  { console.log("Reconocio : " + yytext);  return 'OR' }
"!"                  { console.log("Reconocio : " + yytext);  return 'NOT' }

/* TERNARY OPERATOR FROM SYSCOMPILER */
"?"                  { console.log("Reconocio : " + yytext);  return 'QMARK' }

/* KEYWORDS */
"evaluar"              { console.log("Reconocio : " + yytext);  return 'EVALUAR' }

"true"                { console.log("Reconocio : " + yytext);  return 'TRUE' }
"false"               { console.log("Reconocio : " + yytext);  return 'FALSE' }
"int"                 { console.log("Reconocio : " + yytext);  return 'INT' }
"double"              { console.log("Reconocio : " + yytext);  return 'DOUBLE' }
"boolean"             { console.log("Reconocio : " + yytext);  return 'BOOLEAN' }
"char"                { console.log("Reconocio : " + yytext);  return 'RCHAR' }
"string"              { console.log("Reconocio : " + yytext);  return 'RSTRING' }

"new"                 { console.log("Reconocio : " + yytext);  return 'NEW' }
"DynamicList"         { console.log("Reconocio : " + yytext);  return 'DLIST' }
"append"              { console.log("Reconocio : " + yytext);  return 'APPEND' }
"getValue"            { console.log("Reconocio : " + yytext);  return 'GETVALUE' }
"setValue"            { console.log("Reconocio : " + yytext);  return 'SETVALUE' }

"if"                  { console.log("Reconocio : " + yytext);  return 'IF' }
"else"                { console.log("Reconocio : " + yytext);  return 'ELSE' }
"switch"              { console.log("Reconocio : " + yytext);  return 'SWITCH' }
"case"                { console.log("Reconocio : " + yytext);  return 'CASE' }
"default"             { console.log("Reconocio : " + yytext);  return 'DEFAULT' }

"while"               { console.log("Reconocio : " + yytext);  return 'WHILE' }
"for"                 { console.log("Reconocio : " + yytext);  return 'FOR' }
"do"                  { console.log("Reconocio : " + yytext);  return 'DO' }

"break"               { console.log("Reconocio : " + yytext);  return 'BREAK' }
"continue"            { console.log("Reconocio : " + yytext);  return 'CONTINUE' }
"return"              { console.log("Reconocio : " + yytext);  return 'RETURN' }

"void"                { console.log("Reconocio : " + yytext);  return 'VOID' }
"WriteLine"           { console.log("Reconocio : " + yytext);  return 'WRLINE' }
"toLower"             { console.log("Reconocio : " + yytext);  return 'TOLOWER' }
"toUpper"             { console.log("Reconocio : " + yytext);  return 'TOUPPER' }
"length"              { console.log("Reconocio : " + yytext);  return 'LENGTH' }
"truncate"            { console.log("Reconocio : " + yytext);  return 'TRUNCATE' }
"round"             { console.log("Reconocio : " + yytext);  return 'ROUND' }
"typeof"             { console.log("Reconocio : " + yytext);  return 'TYPEOF' }
"toString"             { console.log("Reconocio : " + yytext);  return 'TOSTRING' }
"toCharArray"             { console.log("Reconocio : " + yytext);  return 'TOCHAR' }
"start"             { console.log("Reconocio : " + yytext);  return 'START' }
"with"             { console.log("Reconocio : " + yytext);  return 'WITH' }

//SIMBOLOS ER

{doublenum} { console.log("Reconocio : " + yytext);  return 'DOUBLENUM' } 
{integer}   { console.log("Reconocio : " + yytext);  return 'INTEGER' }
{id}   { console.log("Reconocio : " + yytext);  return 'ID' }
{string}   { console.log("Reconocio : " + yytext);  return 'STRING' }
{character}   { console.log("Reconocio : " + yytext);  return 'CHAR' }

/*Espacios*/
[\s\r\n\t]             {/* Espacios se ignoran */}


<<EOF>>               return 'EOF'
.                     return 'ERROR'

/lex

// area de imports
%{
    const evaluar = require('../Interpreter/Evaluar');
%}

/* operator associations and precedence */
// LOWER TO HIGHER PRECEDENCCE

%left 'OR'
%left 'AND'

%left 'EQUALTO', 'NOTEQUAL'

%left 'LESSTHAN', 'LESSEQUAL', 'GREATERTHAN', 'GREATEREQUAL'

%left 'PLUS' 'MINUS'
%left 'MULTI' 'DIV', 'MOD'
%right 'POT'
%right 'NOT'
%left UMINUS

%start inicio

%% /* language grammar */

inicio : instrucciones EOF  { $$ = $1; return $$ };

instrucciones : instrucciones instruccion   { $$ = $1; $$.push($2); }
            | instruccion                   { $$ = new Array(); $$.push($1); }
            ;

instruccion : EVALUAR LSBRACKET e RSBRACKET SEMICOLON   { $$ =  new evaluar.default($3); }
            ;
// console : CONSOLE PUNTO LOG '(' e ')' { console.log("HERE is console instruction"); }
//         ;
e
    : e PLUS e
        {$$ = $1 + $3;}
    | e MINUS e
        {$$ = $1-$3;}
    | e MULTI e
        {$$ = $1*$3;}
    | e DIV e
        {$$ = $1/$3;}
    | e POT e
        {$$ = Math.pow($1, $3);}
    | e NOT 
        {{
          $$ = (function fact (n) { return n==0 ? 1 : fact(n-1) * n })($1);
        }}
    | e MOD
        {$$ = $1/100;}
    | MINUS e %prec UMINUS  
        {$$ = -$2;}
    | LPAR e RPAR 
        {$$ = $2;}
    | INTEGER
        {$$ = Number(yytext);}
    | E
        {$$ = Math.E;}
    | PI
        {$$ = Math.PI;}
    ;

