# Nomes da dupla:
# Nome 1: Pedro Lozer
# Nome 2: Marcos Bruno

function lagrange(xk :: Vector{<:Real}, yk :: Vector{<:Real}, x :: Real)
    n = length(xk)
    n == length(yk) || throw(DimensionMismatch("xk e yk devem ter o mesmo tamanho"))
    n > 0 || throw(ArgumentError("os vetores de dados nao podem ser vazios"))

    for i in 1:n, j in (i + 1):n
        xk[i] == xk[j] && throw(ArgumentError("os valores de xk devem ser distintos"))
    end

    T = promote_type(float(eltype(xk)), float(eltype(yk)), float(typeof(x)))
    valor = zero(T)

    for k in 1:n
        Lk = one(T)
        for j in 1:n
            if j != k
                Lk *= (T(x) - T(xk[j])) / (T(xk[k]) - T(xk[j]))
            end
        end
        valor += T(yk[k]) * Lk
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
            println("Entrada invalida. Use numeros separados por virgula, por exemplo: 1, 4, 9.")
        end
    end
end

function escolher_dados()
    while true
        println("\n=== Painel de opcoes ===")
        println("1 - Usar os dados do exemplo")
        println("2 - Informar meus proprios dados")
        print("Escolha uma opcao [1]: ")
        opcao = strip(readline())

        if isempty(opcao) || opcao == "1"
            return [1.0, 4.0, 9.0], [1.0, 2.0, 3.0], 2.0, true
        elseif opcao == "2"
            xk = ler_vetor("xk")
            yk = ler_vetor("yk")
            print("Informe o ponto x: ")
            x = parse(Float64, strip(readline()))
            return xk, yk, x, false
        else
            println("Opcao invalida. Escolha 1 ou 2.")
        end
    end
end

# Painel interativo: permite usar o exemplo ou informar outros dados.
xk, yk, x, usar_exemplo = escolher_dados()
resultado = lagrange(xk, yk, x)
println("\nResultado de lagrange(xk, yk, x): ", resultado)

if usar_exemplo
    @assert isapprox(resultado, 1.366667; atol=1e-6)
    println("Teste do exemplo aprovado.")
end
