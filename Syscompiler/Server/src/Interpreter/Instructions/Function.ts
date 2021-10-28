import AstNode from "../Ast/AstNode";
import Controller from "../Controller";
import { Instruction } from "../Interfaces/Instruction";
import Symbol, {SymbolType} from "../SymbolTable/Symbol";
import SymbolTable from "../SymbolTable/SymbolTable";
import Type from "../SymbolTable/Type";

export default class Function extends Symbol implements Instruction{
    
    public instructionList : Array<Instruction>;
    public line : number;
    public column : number;

//constructor(symbolType : SymbolType, type : Type, id : string, value : any)
    constructor(symbolType : SymbolType, type : Type, identifier : string,  paramList: Array<Symbol>, isMethod : boolean, instructionList : Array<Instruction>, line : number , column : number){
        super(symbolType, type, identifier,null,paramList,isMethod);
        this.instructionList = instructionList;
        this.line = line;
        this.column = column;
    }

    addFunctionST(st : SymbolTable){
        console.log(`guardamos ${this.type.toString()} ${this.id}`);
        if (st.exist(this.id) == false) {
            st.add(this.id,this);
        }else{
            // error semantico
        }
    }

    execute(controller: Controller, symbolTable: SymbolTable) {
        let st_local = new SymbolTable(symbolTable);
        console.log("Estamos dentro de funcion");

        for(let inst of this.instructionList){
            let ret = inst.execute(controller, st_local);

            if(ret != null){
                return ret;
            }
        }
        return null;
    }
    run(): AstNode {
        throw new Error("Method not implemented.");
    }
}