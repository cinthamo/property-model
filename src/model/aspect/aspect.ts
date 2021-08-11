import Resolver from "../resolver/Resolver"
import Contract from "../type/contract"

class Aspect {
    meta: Contract
    definition: Contract
    resolvers: Array<Resolver>
}

export default Aspect