import Resolver from "../resolver/Resolver"
import Contract from "../type/contract"
import Type from "../type/type"
import PropertiesBag from "./PropertiesBag"
import PropertiesObject from "./PropertiesObject"
import PropertyDefinition from "./propertyDefinition"

class PropertiesWithResolver extends PropertiesBag {
    private resolver: Resolver

    constructor(resolver: Resolver) {
        super()
        this.resolver = resolver
    }

    private getDefinition(name: string): PropertyDefinition {
        const type: Type = super.get(META_TYPE)
        if (type !== undefined) {
            const definition = type.getDefinition(name)
            if (definition !== undefined)
                return definition
        }

        const contracts: Array<Contract> = super.get(META_CONTRACTS)
        if (contracts !== undefined) {
            const definition = contracts
                .map((contract: Contract) => contract.getDefinition(name))
                .find((definition: PropertyDefinition) => definition !== undefined)
            if (definition !== undefined)
                return definition
        }

        return undefined
    }

    private getContext(name: string): PropertiesObject {
        const context = new PropertiesBag()
        context.set(CONTEXT_INSTANCE, this)
        context.set(CONTEXT_DEFINITION, this.getDefinition(name))
        return context
    }

    public get(name: string) {
        const context = this.getContext(name)
        const beforeResult = this.resolver.beforeGet(context, name)
        if (beforeResult.resolved)
            return beforeResult.value

        const value = super.get(name)

        const afterResult = this.resolver.afterGet(context, name, value)
        if (afterResult.resolved)
            return afterResult.value

        return value
    }

    public set(name: string, value: any): void {
        const context = this.getContext(name)
        const beforeResult = this.resolver.beforeSet(context, name, value)
        if (beforeResult.resolved) {
            if (beforeResult.cancel)
                return
            else
                value = beforeResult.value
        }

        super.set(name, value)

        this.resolver.afterSet(context, name, value)
    }

    public has(name: string): boolean {
        const context = this.getContext(name)
        const beforeResult = this.resolver.beforeHas(context, name)
        if (beforeResult.resolved)
            return beforeResult.value

        const hasIt = super.has(name)

        const afterResult = this.resolver.afterHas(context, name, hasIt)
        if (afterResult.resolved)
            return beforeResult.value

        return hasIt
    }

    public delete(name: string): void {
        const context = this.getContext(name)
        const beforeResult = this.resolver.beforeDelete(context, name)
        if (beforeResult.resolved) {
            if (beforeResult.cancel)
                return
        }

        super.delete(name)

        this.resolver.afterDelete(context, name)
    }
}

export default PropertiesWithResolver