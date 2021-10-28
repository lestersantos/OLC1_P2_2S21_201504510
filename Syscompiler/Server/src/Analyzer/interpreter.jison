
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
escape      \\{escapechar}
acceptance  [^\"\\]
string      (\"({escape} | {acceptance})*\")

//--> CHARACTER
escapechar2 [\'\"\\ntr"]
escape2     \\{escapechar2}
acceptance2 [^\'\\]
character   (\'({escape2} | {acceptance2})\')

%%

/* Comentarios */
"//".*              {/*Ignoramos los comentarios simples*/ console.log("Reconocio: "+ yytext+" Comentario"); }   id++
"/*"((\*+[^/*])|([^*]))*\**"*/" {/*ignorar comentarios con multiples lineas*/ console.log("Reconocio: "+ yytext+" Comentario multiple");}

/* Simbolos del programa */
"++"                  { console.log("Reconocio : " + yytext);  return 'PLUSPLUS' }
"--"                  { console.log("Reconocio : " + yytext);  return 'MINUSMINUS' }
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

{doublenum} { console.log("Reconocio : " + yytext + " Doble");  return 'DOUBLENUM' } 
{integer}   { console.log("Reconocio : " + yytext + " Entero");  return 'INTEGER' }
{id}   { console.log("Reconocio : " + yytext+ " Id");  return 'ID' }
{string}   { console.log("Reconocio : " + yytext+ " Cadena");  return 'STRING' }
{character}   { console.log("Reconocio : " + yytext+ " Caracter");  return 'CHAR' }

/*Espacios*/
[\s\r\n\t]             {/* Espacios se ignoran */}


<<EOF>>               return 'EOF'
.                     return 'ERROR'

/lex

// area de imports
%{
    const ast = require('../Interpreter/Ast/Ast');

    const Type = require('../Interpreter/SymbolTable/Type');
    const {enumType} = require('../Interpreter/SymbolTable/Type');
    const Symbol = require('../Interpreter/SymbolTable/Symbol');
    const {SymbolType} = require('../Interpreter/SymbolTable/Symbol');

    const Division = require('../Interpreter/Expressions/Arithmetic/Division');
    const Multiplication = require('../Interpreter/Expressions/Arithmetic/Multiplication');    
    const Sum = require('../Interpreter/Expressions/Arithmetic/Sum');  
    const Subtraction = require('../Interpreter/Expressions/Arithmetic/Subtraction');
    const Exponentiation = require('../Interpreter/Expressions/Arithmetic/Exponentiation');
    const Unary = require('../Interpreter/Expressions/Arithmetic/Unary');
    const Modulus = require('../Interpreter/Expressions/Arithmetic/Modulus');

    const And = require('../Interpreter/Expressions/Logic/And');
    const Not = require('../Interpreter/Expressions/Logic/Not');
    const Or = require('../Interpreter/Expressions/Logic/Or');

    const EqualTo = require('../Interpreter/Expressions/Relational/EqualTo');
    const GreaterEqual = require('../Interpreter/Expressions/Relational/GreaterEqual');
    const GreaterThan = require('../Interpreter/Expressions/Relational/GreaterThan');
    const LessEqual = require('../Interpreter/Expressions/Relational/LessEqual');
    const LessThan = require('../Interpreter/Expressions/Relational/LessThan');
    const NotEqual = require('../Interpreter/Expressions/Relational/NotEqual');

    const Literal = require('../Interpreter/Expressions/Literal');
    const Identifier = require('../Interpreter/Expressions/Identifier');

    const WriteLine = require('../Interpreter/Instructions/WriteLine');
    const Declaration = require('../Interpreter/Instructions/Declaration');
    const Assignment = require('../Interpreter/Instructions/Assignment');
    const For = require('../Interpreter/Instructions/LoopStatements/For');
    const While = require('../Interpreter/Instructions/LoopStatements/While');

    const Ifs = require('../Interpreter/Instructions/ControlStatements/Ifs');
    const Case = require('../Interpreter/Instructions/ControlStatements/Case');
    const Switch = require('../Interpreter/Instructions/ControlStatements/Switch');

    const Break = require('../Interpreter/Instructions/TransferStatements/Break');
    const Continue = require('../Interpreter/Instructions/TransferStatements/Continue');
    const Return = require('../Interpreter/Instructions/TransferStatements/Return');

    const Function = require('../Interpreter/Instructions/Function');
    const Call = require('../Interpreter/Instructions/Call');



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

inicio : instrucciones EOF  { $$ = new ast.default($1); return $$ };

instrucciones : instrucciones instruccion   { $$ = $1; $$.push($2); }
            | instruccion                   { $$ = new Array(); $$.push($1);}
            ;

instruccion : writeline            { $$ = $1; }
            | variable_declaration SEMICOLON { $$ = $1; }
            | variable_assignment  SEMICOLON { $$ = $1; }
            | if_statement         { $$ = $1; }
            | for_statement        { $$ = $1; }
            | while_statement      { $$ = $1; }
            | switch_statement     { $$ = $1; }
            | post_increment  SEMICOLON     { $$ = $1; }
            | post_decrement  SEMICOLON     { $$ = $1; }
            | functions             {$$ = $1; }
            | func_call SEMICOLON   {$$ = $1; }
            | BREAK SEMICOLON       { $$ = new Break.default(); }
            | CONTINUE SEMICOLON      { $$ = new Break.default(); }
            | RETURN SEMICOLON      { $$ = new Return.default(null); }
            | RETURN e SEMICOLON      { $$ = new Return.default($2); }
            ;

variable_declaration  : decl_type id_list EQUAL e  {$$ = new Declaration.default($1,$2,$4,@1.first_line,@1.last_column);}
                      | decl_type id_list  {$$ = new Declaration.default($1,$2,null,@1.first_line,@1.last_column);}
                      ;

id_list : id_list COMMA ID {$$ = $1; $$.push($3); }
        | ID               { $$ = new Array(); $$.push($1); }
        ;

decl_type   : INT       {$$ = new Type.default(enumType.INTEGER);}
            | DOUBLE    {$$ = new Type.default(enumType.DOUBLE);}
            | BOOLEAN   {$$ = new Type.default(enumType.BOOLEAN);}
            | RCHAR     {$$ = new Type.default(enumType.CHAR);}
            | RSTRING   {$$ = new Type.default(enumType.STRING);}
            ;

writeline : WRLINE LPAR e RPAR SEMICOLON {$$ = new WriteLine.default($3); }
            ;

variable_assignment : ID EQUAL e  { $$ = new Assignment.default($1,$3,@1.first_line,@1.last_column); }
                      ;

if_statement :  IF LPAR e RPAR LCBRACKET instrucciones RCBRACKET { $$ = new Ifs.default($3,$6,[],@1.first_line,@1.last_column); }
                | IF LPAR e RPAR LCBRACKET instrucciones RCBRACKET ELSE LCBRACKET instrucciones RCBRACKET {$$ = new Ifs.default($3,$6,$10,@1.first_line,@1.last_column);}
                | IF LPAR e RPAR LCBRACKET instrucciones RCBRACKET ELSE if_statement {$$ = new Ifs.default($3,$6,[$9],@1.first_line,@1.last_column);}
              ;

for_statement : FOR LPAR for_init_opt SEMICOLON e SEMICOLON for_update RPAR LCBRACKET instrucciones RCBRACKET { $$ = new For.default($3,$5,$7,$10,@1.first_line,@1.last_column); }
                ;

for_init_opt :   variable_assignment    {$$ = $1;}
                | variable_declaration  {$$ = $1;}
                ;

for_update :    post_increment { $$ = $1; }
              | post_decrement { $$ = $1; }
              | variable_assignment { $$ = $1; }
              ;

while_statement : WHILE LPAR e RPAR LCBRACKET instrucciones RCBRACKET { $$ = new While.default($3, $6, @1.first_line, @1.last_column); }
                  ;

switch_statement :  SWITCH LPAR e RPAR LCBRACKET case_list RCBRACKET         { $$ = new Switch.default($3, $6, null, @1.first_line, @1.last_column); }
                  | SWITCH LPAR e RPAR LCBRACKET case_list default RCBRACKET { $$ = new Switch.default($3, $6, $7, @1.first_line, @1.last_column); }
                  | SWITCH LPAR e RPAR LCBRACKET default RCBRACKET           { $$ = new Switch.default($3, [], $6, @1.first_line, @1.last_column); }
                  ;

case_list :   case_list case  { $$ = $1; $$.push($2); }
            | case            { $$ = new Array(); $$.push($1); }
            ;

case : CASE e COLON instrucciones { $$ = new Case.default($2,$4,@1.first_line, @1.last_column ); }
      ;

default : DEFAULT COLON instrucciones { $$ = new Case.default(null,$3,@1.first_line, @1.last_column ); }
          ;

functions : decl_type ID LPAR params_list RPAR LCBRACKET instrucciones RCBRACKET { $$ = new Function.default(SymbolType.FUNCTION,$1,$2,$4,false,$7, @1.first_line, @1.last_column); }
            | decl_type ID LPAR RPAR LCBRACKET instrucciones RCBRACKET {$$ = new Function.default(SymbolType.FUNCTION,$1,$2,[],false,$6, @1.first_line, @1.last_column);}
            | VOID ID LPAR params_list RPAR LCBRACKET instrucciones RCBRACKET {$$ = new Function.default(SymbolType.METHOD,$1,$2,$4,true,$7, @1.first_line, @1.last_column);}
            | VOID ID LPAR RPAR LCBRACKET instrucciones RCBRACKET {$$ = new Function.default(SymbolType.METHOD,$1,$2,[],true,$6, @1.first_line, @1.last_column);}
            ;

params_list :   params_list COMMA decl_type ID {$$ = $1; $$.push(new Symbol.default(SymbolType.PARAMETER, $3, $4, null)); }
              | decl_type ID { $$ = new Array(); $$.push(new Symbol.default(SymbolType.PARAMETER, $1, $2, null)); }
              ;

func_call :   ID LPAR value_List RPAR { $$ = new Call.default($1,$3,@1.first_line, @1.last_column); }
            | ID LPAR RPAR            { $$ = new Call.default($1,[], @1.first_line, @1.last_column); }
            ;

value_List :   value_List COMMA e     {$$ = $1; $$.push($3); }
              | e                     {$$ = new Array(); $$.push($1); }
             ;
post_increment  : ID PLUSPLUS  { $$ = new Assignment.default($1,new Sum.default(new Identifier.default($1, @1.first_line, @1.last_column),new Literal.default(1,enumType.INTEGER), @1.first_line, @1.last_column),@1.first_line,@1.last_column); }
                ;

post_decrement  : ID MINUSMINUS { $$ = new Assignment.default($1,new Subtraction.default(new Identifier.default($1, @1.first_line, @1.last_column),new Literal.default(1,enumType.INTEGER), @1.first_line, @1.last_column),@1.first_line,@1.last_column); }
                ;

pre_increment   : PLUSPLUS ID %prec UMINUS
                ;
pre_decrement   : MINUSMINUS ID %prec UMINUS
                ;
e
    : e PLUS e              { $$ = new Sum.default($1, $3, @1.first_line, @1.last_column); }
    | e MINUS e             { $$ = new Subtraction.default($1, $3, @1.first_line, @1.last_column); }
    | e MULTI e             { $$ = new Multiplication.default($1, $3, @1.first_line, @1.last_column); }
    | e DIV e               { $$ = new Division.default($1, $3, @1.first_line, @1.last_column); }
    | e POT e               { $$ = new Exponentiation.default($1, $3, @1.first_line, @1.last_column);}
    | e MOD e               { $$ = new Modulus.default($1, $3, @1.first_line, @1.last_column); }
    | post_increment        { $$ = $1; }
    | post_decrement        { $$ = $1; }
    | pre_increment         {}
    | pre_decrement         {}
    | e GREATERTHAN e       { $$ = new GreaterThan.default($1, $3, @1.first_line, @1.last_column); }
    | e GREATEREQUAL e      { $$ = new GreaterEqual.default($1, $3, @1.first_line, @1.last_column); }
    | e LESSTHAN e          { $$ = new LessThan.default($1, $3, @1.first_line, @1.last_column); }
    | e LESSEQUAL e         { $$ = new LessEqual.default($1, $3, @1.first_line, @1.last_column); }
    | e EQUALTO e           { $$ = new EqualTo.default($1, $3, @1.first_line, @1.last_column); }
    | e NOTEQUAL e          { $$ = new NotEqual.default($1, $3, @1.first_line, @1.last_column); }
    | e AND e               { $$ = new And.default($1, $3, @1.first_line, @1.last_column); }
    | e OR e                { $$ = new Or.default($1, $3, @1.first_line, @1.last_column); }
    | NOT e                 { $$ = new Not.default($2, @1.first_line, @1.last_column);}
    | LPAR e RPAR           { $$ = $2; }    
    | MINUS e %prec UMINUS  { $$ = new Unary.default($2, @1.first_line, @1.last_column);}
    | INTEGER               { $$ = new Literal.default(Number($1),enumType.INTEGER); }
    | DOUBLENUM             { $$ = new Literal.default(Number($1),enumType.DOUBLE); }
    | STRING                { $1 = $1.slice(1,$1.length-1); $$ = new Literal.default($1,enumType.STRING); }
    | CHAR                  { $1 = $1.slice(1,$1.length-1); $$ = new Literal.default($1,enumType.CHAR); }
    | ID                    { $$ = new Identifier.default($1, @1.first_line, @1.last_column); }
    | TRUE                  { $$ = new Literal.default(true,enumType.BOOLEAN); }
    | FALSE                 { $$ = new Literal.default(false,enumType.BOOLEAN); }
    | func_call              {$$ = $1; }
    ;

