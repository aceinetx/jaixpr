DEBUG_MEMORY :: false;

Evaluator :: struct {
    pos: s64 = 0;
    code: string;
}

TokenTag :: enum {
    EOF;
    NUMBER;
    PLUS;
    MINUS;
    MUL;
    DIV;
    LPAREN;
    RPAREN;
}

Token :: struct {
    tag: TokenTag;
    number: s64 = 0;
}

advance :: (e: *Evaluator) {
    if e.pos > e.code.count return;
    e.pos += 1;
}

ch :: (e: *Evaluator) -> u8 {
    if e.pos >= e.code.count return 0;
    return e.code[e.pos];
}

next :: (e: *Evaluator) -> (token: Token, success: bool #must) {
    while (true) {
        c := ch(e);
        if !c break;

        if is_space(c) {
            advance(e);
        } else if (is_digit(c)) {
            tok := Token.{ tag = .NUMBER };

            while (is_digit(ch(e))) {
                tok.number *= 10;
                tok.number += ch(e) - #char "0";

                advance(e);
            }

            return tok, true;
        } else if (c == #char "+") {
            advance(e);
            return Token.{ tag = .PLUS }, true;
        } else if (c == #char "-") {
            advance(e);
            return Token.{ tag = .MINUS }, true;
        } else if (c == #char "*") {
            advance(e);
            return Token.{ tag = .MUL }, true;
        } else if (c == #char "/") {
            advance(e);
            return Token.{ tag = .DIV }, true;
        } else if (c == #char "(") {
            advance(e);
            return Token.{ tag = .LPAREN }, true;
        } else if (c == #char ")") {
            advance(e);
            return Token.{ tag = .RPAREN }, true;
        } else {
            return Token.{}, false;
        }
    }

    return .{
        tag = .EOF,
    }, true;
}

peek :: (e: *Evaluator) -> (token: Token, success: bool #must) {
    pos := e.pos;

    token, success := next(e);
    if !success return token, false;

    e.pos = pos;
    return token, true;
}

primary :: (e: *Evaluator) -> (s64, success: bool #must) {
    token, success := next(e);
    if !success return 0, false;

    if token.tag == {
        case .LPAREN;
            v, success := additive(e);
            if !success return 0, false;

            token, success = next(e);
            if !success return 0, false;

            if token.tag != .RPAREN return 0, false;

            return v, true;
        case .NUMBER;
            return token.number, true;
        case;
            return 0, false;
    }
}

multiplicative :: (e: *Evaluator) -> (s64, success: bool #must) {
    lhs, success := primary(e);
    if !success return 0, false;

    while (true) {
        token, success := peek(e);
        if !success return 0, false;

        if token.tag == {
            case .MUL;
                _, success = next(e);
                if !success return 0, false;

                rhs, success := primary(e);
                if !success return 0, false;

                lhs *= rhs;
            case .DIV;
                _, success = next(e);
                if !success return 0, false;

                rhs, success := primary(e);
                if !success return 0, false;

                lhs /= rhs;
            case;
                break;
        }
    }

    return lhs, true;
}

additive :: (e: *Evaluator) -> (s64, success: bool #must) {
    lhs, success := multiplicative(e);
    if !success return 0, false;

    while (true) {
        token, success := peek(e);
        if !success return 0, false;

        if token.tag == {
            case .PLUS;
                _, success = next(e);
                if !success return 0, false;

                rhs, success := multiplicative(e);
                if !success return 0, false;

                lhs += rhs;
            case .MINUS;
                _, success = next(e);
                if !success return 0, false;

                rhs, success := multiplicative(e);
                if !success return 0, false;

                lhs -= rhs;
            case;
                break;
        }
    }

    return lhs, true;
}

main :: () {
    #if DEBUG_MEMORY {
        defer {
            report := make_leak_report();
            log_leak_report(report);
        }
    }

    args := get_command_line_arguments();
    array_unordered_remove_by_index(*args, 0);
    defer array_free(args);

    code := join(..args, "");
    defer free(code);

    e := Evaluator.{ code = code };

    v, success := additive(*e);
    print("% = ", code);
    if success {
        print("%\n", v);
    } else {
        print("eval failed\n");
    }
}

#import "Basic"()(MEMORY_DEBUGGER=DEBUG_MEMORY);
#import "String";
