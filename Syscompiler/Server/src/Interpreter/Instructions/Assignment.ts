import AstNode from "../Ast/AstNode";
import SysError from "../Ast/SysError";
import Controller from "../Controller";
import Expression from "../Interfaces/Expression";
import { Instruction } from "../Interfaces/Instruction";
import SymbolTable from "../SymbolTable/SymbolTable";

export default class Assignment implements Instruction {

    public identifier: string;
    public expression: Expression;
    public line: number;
    public column: number;

    constructor(identifier: string, expression: Expression, line: number, column: number) {
        this.identifier = identifier;
        this.expression = expression;
        this.line = line;
        this.column = column;
    }

    execute(controller: Controller, symbolTable: SymbolTable) {

        if (symbolTable.exist(this.identifier) == true) {

            let resExpr = this.expression.getValue(controller, symbolTable);
            let variable = symbolTable.get(this.identifier);

            if (variable?.type.getTypeName() == resExpr.type.getTypeName()) {
                variable.setValue(resExpr.value);
            } else {
                let error = new SysError("Semantico", `Incompatibilidad ${this.identifier}: tipo ${variable?.type.toString()} no puede asignarse tipo ${resExpr.type.toString()}`, this.line, this.column);
                controller.addError(error);
                controller.append(` ***ERROR: Incompatibilidad ${this.identifier}: tipo ${variable?.type.toString()} no puede asignarse tipo ${resExpr.type.toString()}En la linea  ${this.line} y columna ${this.column}`);
            }
        } else {
            let error = new SysError("Semantico", `La variable ${this.identifier} no existe en la tabla de simbolos `, this.line, this.column);
            controller.addError(error);
            controller.append(` ***ERROR: La variable ${this.identifier} no existe en la tabla de simbolos. En la linea ${this.line} y columna ${this.column}`);
        }
    }

    run(): AstNode {
        throw new Error("Method not implemented.");
    }
}