import PropertyDefinition from "../property/propertyDefinition"
import Contract from "./contract"

class Type {
    super: Type
    contracts: Array<Contract>

    getDefinition(name: string): PropertyDefinition {
        throw new Error("Method not implemented.")
    }
}

export default Type