import PropertiesObject from "../property/PropertiesObject"

type GetValueResult = { resolved: boolean, value?: any }
type BeforeSetValueResult = { resolved: boolean, cancel?: boolean, value?: any }
type AfterSetValueResult = { resolved: boolean }

interface Resolver {
    beforeGet(context: PropertiesObject, name: string): GetValueResult
    afterGet(context: PropertiesObject, name: string, value: any): GetValueResult
    beforeSet(context: PropertiesObject, name: string, value: any): BeforeSetValueResult
    afterSet(context: PropertiesObject, name: string, value: any): AfterSetValueResult
    beforeHas(context: PropertiesObject, name: string): GetValueResult
    afterHas(context: PropertiesObject, name: string, hasIt: boolean): GetValueResult
    beforeDelete(context: PropertiesObject, name: string): BeforeSetValueResult
    afterDelete(context: PropertiesObject, name: string): AfterSetValueResult
}

export default Resolver