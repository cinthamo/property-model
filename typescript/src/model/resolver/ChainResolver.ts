import PropertiesObject from "../property/PropertiesObject"
import Resolver, { AfterSetValueResult, BeforeSetValueResult, GetValueResult } from "./Resolver"

class ChainResolver implements Resolver {
    private chain: Array<Resolver>

    constructor(chain: Array<Resolver>) {
        this.chain = chain
    }

    beforeGet(context: PropertiesObject, name: string): GetValueResult {
        throw new Error("Method not implemented.")
    }

    afterGet(context: PropertiesObject, name: string, value: any): GetValueResult {
        throw new Error("Method not implemented.")
    }

    beforeSet(context: PropertiesObject, name: string, value: any): BeforeSetValueResult {
        throw new Error("Method not implemented.")
    }

    afterSet(context: PropertiesObject, name: string, value: any): AfterSetValueResult {
        throw new Error("Method not implemented.")
    }

    beforeHas(context: PropertiesObject, name: string): GetValueResult {
        throw new Error("Method not implemented.")
    }

    afterHas(context: PropertiesObject, name: string, hasIt: boolean): GetValueResult {
        throw new Error("Method not implemented.")
    }

    beforeDelete(context: PropertiesObject, name: string): BeforeSetValueResult {
        throw new Error("Method not implemented.")
    }

    afterDelete(context: PropertiesObject, name: string): AfterSetValueResult {
        throw new Error("Method not implemented.")
    }
}

export default ChainResolver