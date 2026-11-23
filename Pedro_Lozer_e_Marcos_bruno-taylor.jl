# Nomes da dupla:
# Nome 1: Pedro Lozer
# Nome 2: Marcos Bruno

function taylor(d :: Vector{<:Real}, x0 :: Real, x :: Real)
    isempty(d) && throw(ArgumentError("d deve conter pelo menos um valor"))

    T = promote_type(float(eltype(d)), float(typeof(x0)), float(typeof(x)))
    dx = T(x) - T(x0)
    valor = zero(T)
    potencia = one(T)
    fatorial = one(T)

    for k in 0:(length(d) - 1)
        if k > 0
            potencia *= dx
            fatorial *= k
        end
        valor += T(d[k + 1]) * potencia / fatorial
    end

    return valor
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
            println("Entrada invalida. Exemplo: 1, 1, 1, 1, 1")
        end
    end
end

function escolher_opcao()
    while true
        println("\n=== Painel de opcoes ===")
        println("1 - Usar o exemplo f(x) = exp(x)")
        println("2 - Usar o exemplo f(x) = sin(x)")
        println("3 - Informar meus proprios dados")
        print("Escolha uma opcao [1]: ")
        opcao = strip(readline())

        if isempty(opcao) || opcao == "1"
            return :exponencial
        elseif opcao == "2"
            return :seno
        elseif opcao == "3"
            return :personalizado
        else
            println("Opcao invalida. Escolha 1, 2 ou 3.")
        end
    end
end

opcao = escolher_opcao()

if opcao == :exponencial
    d = [1.0, 1.0, 1.0, 1.0, 1.0]
    x0 = 0.0
    x = 1.0
    aproximacao = taylor(d, x0, x)
    valor_exato = exp(x)

    println("\nTaylor de grau 4 para exp(x): ", aproximacao)
    println("Valor de exp(1): ", valor_exato)
    println("Erro absoluto: ", abs(aproximacao - valor_exato))
    @assert isapprox(aproximacao, 2.708333; atol=1e-6)

elseif opcao == :seno
    d = [0.0, 1.0, 0.0, -1.0, 0.0, 1.0]
    x0 = 0.0
    x = 0.5
    aproximacao = taylor(d, x0, x)
    valor_exato = sin(x)

    println("\nTaylor de grau 5 para sin(x): ", aproximacao)
    println("Valor de sin(0.5): ", valor_exato)
    println("Erro absoluto: ", abs(aproximacao - valor_exato))
    @assert isapprox(aproximacao, 0.479427; atol=1e-6)

else
    d = ler_vetor("d (f(x0), f'(x0), f''(x0), ...)")
    print("Centro x0: ")
    x0 = parse(Float64, strip(readline()))
    print("Ponto x para avaliar: ")
    x = parse(Float64, strip(readline()))

    aproximacao = taylor(d, x0, x)
    println("\nGrau do polinomio: ", length(d) - 1)
    println("Valor aproximado do polinomio de Taylor: ", aproximacao)
end
