import Resolver from "../resolver/Resolver"
import Contract from "../type/contract"

class Behaviour {
    meta: Contract
    definition: Contract
    resolvers: Array<Resolver>
}

export default Behaviour