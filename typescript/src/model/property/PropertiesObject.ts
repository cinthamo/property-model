interface PropertiesObject {
    get(name: string): any
    set(name: string, value: any): void
    has(name: string): boolean
    delete(name: string): void
}

export default PropertiesObject