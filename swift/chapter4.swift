import Foundation

public enum term : CustomStringConvertible {
    case TmTrue
    case TmFalse
    indirect case TmIf(term, term, term)
    case TmZero
    indirect case TmSucc(term)
    indirect case TmPred(term)
    indirect case TmIsZero(term)

    public var description : String {
        get {
            switch self {
            case .TmTrue: return "true"
            case .TmFalse: return "false"
            case .TmIf(let t1, let t2, let t3): return "if " + String(t1) + " then " + String(t2) + " else " + String(t3)
            case .TmZero: return "0"
            case .TmSucc(let t): return "succ(" + String(t) + ")"
            case .TmPred(let t): return "pred(" + String(t) + ")"
            case .TmIsZero(let t): return "iszero(" + String(t) + ")"
            }
        }
    }
}

public func isnumericval(t: term) -> Bool {
    switch t {
    case .TmZero: return true
    case .TmSucc(let t1): return isnumericval(t1)
    default: return false
    }
}

public func isval(t: term) -> Bool {
    switch t {
    case .TmTrue: return true
    case .TmFalse: return true
    case let t1 where isnumericval(t1): return true
    default: return false
    }
}

enum EvalError : ErrorType {
    case NoRuleApplies
}

public func eval1(t: term) throws -> term {
    switch t {
    case .TmIf(.TmTrue, let t2, _):
        return t2
    case .TmIf(.TmFalse, _, let t3):
        return t3
    case .TmIf(let t1, let t2, let t3):
        let _t1 = try eval1(t1)
        return .TmIf(_t1, t2, t3)
    case .TmSucc(let t1):
        let _t1 = try eval1(t1)
        return .TmSucc(_t1)
    case .TmPred(.TmZero):
        return .TmZero
    case .TmPred(.TmSucc(let nv1)) where isnumericval(nv1):
        return nv1
    case .TmPred(let t1):
        let _t1 = try eval1(t1)
        return .TmPred(_t1)
    case .TmIsZero(.TmZero):
        return .TmTrue
    case .TmIsZero(.TmSucc(let nv1)) where isnumericval(nv1):
        return .TmFalse
    case .TmIsZero(let t1):
        let _t1 = try eval1(t1)
        return .TmIsZero(_t1)
    default:
        throw EvalError.NoRuleApplies
    }
}

public func eval(t: term) -> term {
    do {
        let t1 = try eval1(t)
        return eval(t1)
    } catch {
        return t
    }
}

public func evalbigstep(t: term) throws -> term {
    switch t {
    case .TmIf(let t1, let t2, let t3):
        let _t1 = try evalbigstep(t1)
        switch _t1 {
        case .TmTrue:
            return try evalbigstep(t2)   // B-IFTRUE
        case .TmFalse:
            return try evalbigstep(t3)   // B-IFFALSE
        default:
            throw EvalError.NoRuleApplies
        }
    case .TmSucc(let t1):
        let _t1 = try evalbigstep(t1)
        switch _t1 {
        case let _t1 where isnumericval(_t1):
            return .TmSucc(_t1)   // B-SUCC
        default:
            throw EvalError.NoRuleApplies
        }
    case .TmPred(let t1):
        let _t1 = try evalbigstep(t1)
        switch _t1 {
        case .TmZero:
            return .TmZero   // B-PREDZERO
        case .TmSucc(let nv1):
            return try evalbigstep(nv1)   // B-PREDSUCC
        default:
            throw EvalError.NoRuleApplies
        }
    case .TmIsZero(let t1):
        let _t1 = try evalbigstep(t1)
        switch _t1 {
        case .TmZero:
            return .TmTrue   // B-ISZEROZERO
        case .TmSucc:
            return .TmFalse   // B-ISZEROSUCC
        default:
            throw EvalError.NoRuleApplies
        }
    default:
        return t   // B-VALUE
    }
}

