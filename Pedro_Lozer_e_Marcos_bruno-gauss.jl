# Nomes da dupla:
# Nome 1: Pedro Lozer
# Nome 2: Marcos Bruno

function legendre_par(n :: Int, t :: Real)
    n >= 0 || throw(ArgumentError("n deve ser nao negativo"))
    if n == 0
        return 1.0, 0.0
    elseif n == 1
        return float(t), 1.0
    end

    p_anterior = 1.0
    p_atual = float(t)
    for k in 1:(n - 1)
        p_proximo = ((2k + 1) * t * p_atual - k * p_anterior) / (k + 1)
        p_anterior, p_atual = p_atual, p_proximo
    end
    return p_atual, p_anterior
end

function gauss(f :: Function, a :: Real, b :: Real, n :: Int)
    2 <= n <= 10 || throw(ArgumentError("n deve estar entre 2 e 10"))

    pontos = Float64[]
    pesos = Float64[]
    tolerancia = 1e-15

    for i in 1:n
        t = cos(pi * (i - 0.25) / (n + 0.5))
        convergiu = false

        for iteracao in 1:100
            pn, pnm1 = legendre_par(n, t)
            derivada = n * (t * pn - pnm1) / (t^2 - 1)
            t_novo = t - pn / derivada

            if abs(t_novo - t) < tolerancia
                t = t_novo
                convergiu = true
                break
            end
            t = t_novo
        end

        convergiu || throw(ErrorException("Newton nao convergiu para um ponto de Gauss"))
        pn, pnm1 = legendre_par(n, t)
        derivada = n * (t * pn - pnm1) / (t^2 - 1)
        peso = 2 / ((1 - t^2) * derivada^2)
        push!(pontos, t)
        push!(pesos, peso)
    end

    ordem = sortperm(pontos)
    pontos = pontos[ordem]
    pesos = pesos[ordem]

    meio = (a + b) / 2
    escala = (b - a) / 2
    soma = sum(pesos[i] * f(meio + escala * pontos[i]) for i in 1:n)
    return escala * soma
end

# Avalia expressoes simples sem executar codigo Julia arbitrario.
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
        throw(ArgumentError("simbolo nao permitido"))
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
    throw(ArgumentError("expressao invalida"))
end

function ler_funcao()
    while true
        print("Digite f(x), por exemplo exp(x), sqrt(x) ou 1/x: ")
        entrada = strip(readline())
        try
            expr = Meta.parse(entrada)
            validar_expressao(expr)
            return x -> avaliar_expressao(expr, x)
        catch
            println("Expressao invalida. Use x, pi, +, -, *, /, ^ ou sin, cos, tan, exp, log, sqrt, abs.")
        end
    end
end

function escolher_opcao()
    while true
        println("\n=== Painel de opcoes ===")
        println("1 - Usar os dois testes do exemplo")
        println("2 - Informar uma funcao, intervalo e numero de pontos")
        print("Escolha uma opcao [1]: ")
        opcao = strip(readline())

        if isempty(opcao) || opcao == "1"
            return :exemplos
        elseif opcao == "2"
            return :personalizado
        else
            println("Opcao invalida. Escolha 1 ou 2.")
        end
    end
end

function mostrar_resultados(f, a, b, ns, valor_exato)
    for n in ns
        aproximacao = gauss(f, a, b, n)
        println("n = ", n, ": ", aproximacao, " (erro absoluto = ", abs(aproximacao - valor_exato), ")")
    end
    println("Valor exato: ", valor_exato)
end

opcao = escolher_opcao()

if opcao == :exemplos
    println("\nTeste 1: f(x) = exp(x), intervalo [0, 1]")
    mostrar_resultados(x -> exp(x), 0.0, 1.0, [2, 3, 5], exp(1) - 1)

    println("\nTeste 2: f(x) = sqrt(x), intervalo [0, 1]")
    mostrar_resultados(x -> sqrt(x), 0.0, 1.0, [2, 5, 10], 2 / 3)

    @assert isapprox(gauss(x -> exp(x), 0.0, 1.0, 2), 1.7178963780; atol=5e-10)
    @assert isapprox(gauss(x -> exp(x), 0.0, 1.0, 3), 1.7182810044; atol=5e-10)
    @assert isapprox(gauss(x -> exp(x), 0.0, 1.0, 5), 1.7182818285; atol=5e-10)
    @assert isapprox(gauss(x -> sqrt(x), 0.0, 1.0, 2), 0.6738873387; atol=5e-10)
    @assert isapprox(gauss(x -> sqrt(x), 0.0, 1.0, 5), 0.6672967897; atol=5e-10)
    @assert isapprox(gauss(x -> sqrt(x), 0.0, 1.0, 10), 0.6667560429; atol=5e-10)
    println("Testes do exemplo aprovados.")

else
    f = ler_funcao()
    print("Limite inferior a: ")
    a = parse(Float64, strip(readline()))
    print("Limite superior b: ")
    b = parse(Float64, strip(readline()))
    print("Numero de pontos de Gauss n (2 a 10): ")
    n = parse(Int, strip(readline()))

    println("\nIntegral aproximada: ", gauss(f, a, b, n))
end
