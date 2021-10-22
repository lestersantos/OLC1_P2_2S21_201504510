import AstNode from "../Ast/AstNode";
import Controller from "../Controller";
import Expression from "../Interfaces/Expression";
import SymbolTable from "../SymbolTable/SymbolTable";
import Type, { enumType } from "../SymbolTable/Type";

export default abstract class UnaryOperation implements Expression{
    public exp1 : Expression;
    public line : number;
    public column : number;
    public type: Type;
    public value: any;

    constructor(exp1 : Expression, line : number, column : number){
        this.exp1 = exp1;
        this.line = line;
        this.column = column;
        this.type = new Type(enumType.ERROR);    
        this.value = undefined;
    }
    abstract  getValue(controller: Controller, symbolTable: SymbolTable): Expression;

    abstract run() : AstNode;
}