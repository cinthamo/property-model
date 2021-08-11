import Resolver from "./Resolver"

class ChainResolver implements Resolver {
    private chain: Array<Resolver>

    constructor(chain: Array<Resolver>) {
        this.chain = chain
    }

    public beforeGet(name: string): GetValueResult {
        this.chain.forEach()
    }

    afterGet(name: string, value: any): GetValueResult
    beforeSet(name: string, value: any): BeforeSetValueResult
    afterSet(name: string, value: any): AfterSetValueResult
    beforeHas(name: string): GetValueResult
    afterHas(name: string, hasIt: boolean): GetValueResult
    beforeDelete(name: string): BeforeSetValueResult
    afterDelete(name: string): AfterSetValueResult
}

export default ChainResolver