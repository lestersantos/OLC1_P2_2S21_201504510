import SysError from "./Ast/SysError";

export default class Controller{
    public errors : Array<SysError>;
    public consoleMsg : string;
    public loopStatement : boolean; 

    constructor(){
        this.errors = new Array<SysError>();
        this.consoleMsg = "";
        this.loopStatement = false;
    }

    append(msg : string){
        this.consoleMsg = this.consoleMsg + msg + "\r\n ";
    }

    setIsLoopStmt(isLoopStmt : boolean){
        this.loopStatement = isLoopStmt;
    }

    isLoopStmt(){
        return this.loopStatement;
    }

    addError(error : SysError): void{
        this.errors.push(error);
    }
}