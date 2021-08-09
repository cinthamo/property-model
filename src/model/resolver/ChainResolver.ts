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
    beforePut(name: string, value: any): BeforeSetValueResult
    afterPut(name: string, value: any): AfterSetValueResult
    beforeHas(name: string): GetValueResult
    afterHas(name: string, hasIt: boolean): GetValueResult
    beforeDelete(name: string): BeforeSetValueResult
    afterDelete(name: string): AfterSetValueResult
}

export default ChainResolver