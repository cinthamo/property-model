import PropertiesObject from "./PropertiesObject";

class PropertiesBag implements PropertiesObject {
    private map = {}

    public get(name: string) {
        return this.map[name]
    }

    public put(name: string, value: any): void {
        this.map[name] = value
    }

    public has(name: string): boolean {
        return Object.keys(this.map).includes(name)
    }

    public delete(name: string): void {
        delete this.map[name]
    }
}

export default PropertiesBag