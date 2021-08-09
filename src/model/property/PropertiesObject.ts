interface PropertiesObject {
    get(name: string): any
    put(name: string, value: any): void
    has(name: string): boolean
    delete(name: string): void
}

export default PropertiesObject