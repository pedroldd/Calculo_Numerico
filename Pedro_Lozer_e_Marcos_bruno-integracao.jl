# Nomes da dupla:
# Nome 1: Pedro Lozer
# Nome 2: Marcos Bruno

function ponto_medio(f :: Function, a :: Real, b :: Real, m :: Int)
    m > 0 || throw(ArgumentError("m deve ser positivo"))
    h = (b - a) / m
    soma = sum(f(a + (i - 0.5) * h) for i in 1:m)
    return h * soma
end

function trapezios(f :: Function, a :: Real, b :: Real, m :: Int)
    m > 0 || throw(ArgumentError("m deve ser positivo"))
    h = (b - a) / m
    soma_interna = sum(f(a + i * h) for i in 1:(m - 1))
    return h * ((f(a) + f(b)) / 2 + soma_interna)
end

function simpson(f :: Function, a :: Real, b :: Real, m :: Int)
    m > 0 || throw(ArgumentError("m deve ser positivo"))
    iseven(m) || throw(ArgumentError("m deve ser par para a regra de Simpson"))
    h = (b - a) / m

    soma_impares = sum(f(a + i * h) for i in 1:2:(m - 1))
    soma_pares = sum(f(a + i * h) for i in 2:2:(m - 2))
    return (h / 3) * (f(a) + 4 * soma_impares + 2 * soma_pares + f(b))
end

function trapezios(y :: Vector{<:Real}, h :: Real)
    length(y) >= 2 || throw(ArgumentError("y deve conter pelo menos dois valores"))
    h != 0 || throw(ArgumentError("h nao pode ser zero"))
    return h * ((y[1] + y[end]) / 2 + sum(y[2:(end - 1)]))
end

function simpson(y :: Vector{<:Real}, h :: Real)
    length(y) >= 3 || throw(ArgumentError("y deve conter pelo menos tres valores"))
    h != 0 || throw(ArgumentError("h nao pode ser zero"))
    m = length(y) - 1
    iseven(m) || throw(ArgumentError("y deve ter um numero impar de valores para Simpson"))

    soma_impares = sum(y[i] for i in 2:2:(m))
    soma_pares = sum(y[i] for i in 3:2:(m - 1))
    return (h / 3) * (y[1] + 4 * soma_impares + 2 * soma_pares + y[end])
end

# Avalia expressoes aritmeticas simples sem executar codigo Julia arbitrario.
function validar_expressao(expr)
    if expr isa Number
        return nothing
    elseif expr isa Symbol
        expr in (:x, :pi) || throw(ArgumentError("simbolo nao permitido"))
        return nothing
    elseif expr isa Expr && expr.head == :call
        operador = expr.args[1]
        permitidos = (:+, :-, :*, :/, :^, :sin, :cos, :tan, :exp, :log, :sqrt, :abs)
        operador in permitidos || throw(ArgumentError("operador ou funcao nao permitido"))
        foreach(validar_expressao, expr.args[2:end])
        return nothing
    end

    throw(ArgumentError("expressao invalida"))
end

function avaliar_expressao(expr, x)
    if expr isa Number
        return expr
    elseif expr isa Symbol
        expr == :x && return x
        expr == :pi && return pi
        throw(ArgumentError("use x, pi, numeros, operadores ou funcoes permitidas"))
    elseif expr isa Expr && expr.head == :call
        operador = expr.args[1]
        args = [avaliar_expressao(arg, x) for arg in expr.args[2:end]]

        operador == :+ && return +(args...)
        operador == :- && return length(args) == 1 ? -args[1] : args[1] - args[2]
        operador == :* && return *(args...)
        operador == :/ && return args[1] / args[2]
        operador == :^ && return args[1] ^ args[2]
        operador == :sin && return sin(args[1])
        operador == :cos && return cos(args[1])
        operador == :tan && return tan(args[1])
        operador == :exp && return exp(args[1])
        operador == :log && return log(args[1])
        operador == :sqrt && return sqrt(args[1])
        operador == :abs && return abs(args[1])
    end

    throw(ArgumentError("expressao invalida ou funcao nao permitida"))
end

function ler_funcao()
    while true
        print("Digite f(x), por exemplo 1/x, x^2 ou sin(x): ")
        entrada = strip(readline())
        try
            expr = Meta.parse(entrada)
            validar_expressao(expr)
            f = x -> avaliar_expressao(expr, x)
            return f
        catch
            println("Expressao invalida. Use x, pi, +, -, *, /, ^ ou sin, cos, tan, exp, log, sqrt, abs.")
        end
    end
end

function ler_vetor(nome)
    while true
        print("Informe os valores de $nome separados por virgula: ")
        entrada = strip(readline())
        try
            valores = parse.(Float64, strip.(split(entrada, ',')))
            isempty(valores) && error("informe pelo menos um valor")
            return valores
        catch
            println("Entrada invalida. Exemplo: 1, 0.8, 0.6")
        end
    end
end

function escolher_opcao()
    while true
        println("\n=== Painel de opcoes ===")
        println("1 - Usar os dados do exemplo")
        println("2 - Informar uma funcao e o intervalo")
        println("3 - Informar valores tabelados igualmente espacados")
        print("Escolha uma opcao [1]: ")
        opcao = strip(readline())

        if isempty(opcao) || opcao == "1"
            return :exemplo
        elseif opcao == "2"
            return :funcao
        elseif opcao == "3"
            return :tabela
        else
            println("Opcao invalida. Escolha 1, 2 ou 3.")
        end
    end
end

opcao = escolher_opcao()

if opcao == :exemplo
    f = x -> 1 / x
    a, b, m = 1.0, 2.0, 4
    y = [1.0, 1 / 1.25, 1 / 1.5, 1 / 1.75, 1 / 2]
    h_tabela = 0.25

    valor_medio = ponto_medio(f, a, b, m)
    valor_trapezios = trapezios(f, a, b, m)
    valor_simpson = simpson(f, a, b, m)
    valor_trapezios_tabela = trapezios(y, h_tabela)
    valor_simpson_tabela = simpson(y, h_tabela)

    println("\nPonto medio: ", valor_medio)
    println("Trapezios: ", valor_trapezios)
    println("Simpson: ", valor_simpson)
    println("Trapezios com y tabelado: ", valor_trapezios_tabela)
    println("Simpson com y tabelado: ", valor_simpson_tabela)
    println("Valor exato ln(2): ", log(2))

    @assert isapprox(valor_medio, 0.691220; atol=1e-6)
    @assert isapprox(valor_trapezios, 0.697024; atol=1e-6)
    @assert isapprox(valor_simpson, 0.693254; atol=1e-6)
    @assert isapprox(valor_trapezios, valor_trapezios_tabela; atol=1e-12)
    @assert isapprox(valor_simpson, valor_simpson_tabela; atol=1e-12)
    println("Teste do exemplo aprovado.")

elseif opcao == :funcao
    f = ler_funcao()
    print("Limite inferior a: ")
    a = parse(Float64, strip(readline()))
    print("Limite superior b: ")
    b = parse(Float64, strip(readline()))
    print("Numero de subintervalos m (par): ")
    m = parse(Int, strip(readline()))

    println("\nPonto medio: ", ponto_medio(f, a, b, m))
    println("Trapezios: ", trapezios(f, a, b, m))
    println("Simpson: ", simpson(f, a, b, m))

else
    y = ler_vetor("y")
    print("Espacamento h: ")
    h = parse(Float64, strip(readline()))

    println("\nTrapezios com y tabelado: ", trapezios(y, h))
    println("Simpson com y tabelado: ", simpson(y, h))
end
