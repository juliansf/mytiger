signature TigerAssem =
sig
    type reg = string
    type temp = TigerTemp.temp
    type label = TigerTemp.label

    datatype instr = OPER of {assem: string,
                              dst: temp list,
                              src: temp list,
                              jump: label list option}
                   | LABEL of {assem: string,
                               lab: label}   (*<-- acá el tipo podría ser label directamente*)
                   | MOVE of {assem: string,
                              dst: temp,
                              src: temp}
                              
    val format : (temp->string) -> instr -> string
end
