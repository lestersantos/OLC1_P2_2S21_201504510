import AstNode from "../../Ast/AstNode";
import Controller from "../../Controller";
import Expression from "../../Interfaces/Expression";
import { Instruction } from "../../Interfaces/Instruction";
import SymbolTable from "../../SymbolTable/SymbolTable";

export default class Return implements Instruction{
    
    public returnValue : Expression;

    constructor(returnValue : Expression){
        this.returnValue = returnValue;
    }

    execute(controller: Controller, symbolTable: SymbolTable) {
        
        if (this.returnValue != null) {
            console.log("Estamos en return");
            console.log(this.returnValue.getValue(controller,symbolTable));
            return this.returnValue.getValue(controller,symbolTable);
        }else{
            this;
        }
    }
    
    run(): AstNode {
        throw new Error("Method not implemented.");
    }
}