import Controller from "../Controller";
import { Instruction } from "../Interfaces/Instruction";
import SymbolTable from "../SymbolTable/SymbolTable";
import Function from "../Instructions/Function";
import AstNode from "./AstNode";
import Declaration from "../Instructions/Declaration";

export default class Ast implements Instruction {

    public instructionList: Array<Instruction>;

    constructor(instructionList: Array<Instruction>) {
        this.instructionList = instructionList;
    }
    execute(controller: Controller, symbolTable: SymbolTable) {

        for (let instruction of this.instructionList) {
            if (instruction instanceof Function) {
                let thisFunction = instruction as Function;

                thisFunction.addFunctionST(symbolTable);
            }
        }

        for(let instruction of this.instructionList){
            if(instruction instanceof Declaration){
                instruction.execute(controller,symbolTable);
            }
        }

        for (let instruction of this.instructionList) {
            if (!(instruction instanceof Function) && !(instruction instanceof Declaration)) {
                instruction.execute(controller, symbolTable);
            }
        }
    }
    run(): AstNode {
        let root = new AstNode("INICIO", "");

        for(let inst of this.instructionList){
            root.addChild(inst.run());
        }

        return root;
    }

}