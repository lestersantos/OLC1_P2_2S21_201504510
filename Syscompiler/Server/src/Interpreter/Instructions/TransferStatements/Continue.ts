import AstNode from "../../Ast/AstNode";
import Controller from "../../Controller";
import { Instruction } from "../../Interfaces/Instruction";
import SymbolTable from "../../SymbolTable/SymbolTable";

export default class Continue implements Instruction{

    constructor(){

    }

    execute(controller: Controller, symbolTable: SymbolTable) {
        return this;
    }
    run(): AstNode {
        throw new Error("Method not implemented.");
    }

}