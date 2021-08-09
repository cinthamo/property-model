type GetValueResult = { resolved: boolean, value?: any }
type BeforeSetValueResult = { resolved: boolean, cancel?: boolean, value?: any }
type AfterSetValueResult = { resolved: boolean }

interface Resolver {
    beforeGet(name: string): GetValueResult
    afterGet(name: string, value: any): GetValueResult
    beforePut(name: string, value: any): BeforeSetValueResult
    afterPut(name: string, value: any): AfterSetValueResult
    beforeHas(name: string): GetValueResult
    afterHas(name: string, hasIt: boolean): GetValueResult
    beforeDelete(name: string): BeforeSetValueResult
    afterDelete(name: string): AfterSetValueResult
}

export default Resolver