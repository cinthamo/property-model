import PropertiesBag from "./PropertiesBag"
import Resolver from "./Resolver"

class PropertiesWithResolver extends PropertiesBag {
    private resolver: Resolver

    constructor(resolver: Resolver) {
        super()
        this.resolver = resolver
    }

    public get(name: string) {
        const beforeResult = this.resolver.beforeGet(name)
        if (beforeResult.resolved)
            return beforeResult.value

        const value = super.get(name)

        const afterResult = this.resolver.afterGet(name, value)
        if (afterResult.resolved)
            return afterResult.value

        return value
    }

    public put(name: string, value: any): void {
        const beforeResult = this.resolver.beforePut(name, value)
        if (beforeResult.resolved) {
            if (beforeResult.cancel)
                return
            else
                value = beforeResult.value
        }

        super.put(name, value)

        this.resolver.afterPut(name, value)
    }

    public has(name: string): boolean {
        const beforeResult = this.resolver.beforeHas(name)
        if (beforeResult.resolved)
            return beforeResult.value

        const hasIt = super.has(name)

        const afterResult = this.resolver.afterHas(name, hasIt)
        if (afterResult.resolved)
            return beforeResult.value

        return hasIt
    }

    public delete(name: string): void {
        const beforeResult = this.resolver.beforeDelete(name)
        if (beforeResult.resolved) {
            if (beforeResult.cancel)
                return
        }

        super.delete(name)

        this.resolver.afterDelete(name)
    }
}

export default PropertiesWithResolver