import Controller from "../Controller";
import { Instruction } from "../Interfaces/Instruction";
import SymbolTable from "../SymbolTable/SymbolTable";
import AstNode from "./AstNode";

export default class Ast implements Instruction{

    public instructionList : Array<Instruction>;

    constructor(instructionList : Array<Instruction>){
        this.instructionList = instructionList;
    }
    execute(controller: Controller, symbolTable: SymbolTable) {
        for(let instruction of this.instructionList){
            instruction.execute(controller, symbolTable);
        }
    }
    run(): AstNode {
        throw new Error("Method not implemented.");
    }

}