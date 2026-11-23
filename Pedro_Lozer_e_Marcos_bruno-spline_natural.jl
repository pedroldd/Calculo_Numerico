# Nomes da dupla:
# Nome 1: Pedro Lozer
# Nome 2: Marcos Bruno

function spline_natural(xk :: Vector{<:Real}, yk :: Vector{<:Real})
    n = length(xk)
    n == length(yk) || throw(DimensionMismatch("xk e yk devem ter o mesmo tamanho"))
    n >= 2 || throw(ArgumentError("informe pelo menos dois pontos"))

    T = promote_type(float(eltype(xk)), float(eltype(yk)))
    x = T.(xk)
    y = T.(yk)

    for i in 1:(n - 1)
        x[i] < x[i + 1] || throw(ArgumentError("os valores de xk devem estar em ordem crescente e ser distintos"))
    end

    h = diff(x)
    b_pontos = zeros(T, n)

    # Resolve o sistema para os coeficientes internos b do spline natural.
    if n > 2
        m = n - 2
        A = zeros(T, m, m)
        rhs = zeros(T, m)

        for j in 1:m
            i = j + 1
            h_esq = h[i - 1]
            h_dir = h[i]

            A[j, j] = 2 * (h_esq + h_dir)
            if j > 1
                A[j, j - 1] = h_esq
            end
            if j < m
                A[j, j + 1] = h_dir
            end

            inclinacao_esq = (y[i] - y[i - 1]) / h_esq
            inclinacao_dir = (y[i + 1] - y[i]) / h_dir
            rhs[j] = 3 * (inclinacao_dir - inclinacao_esq)
        end

        b_pontos[2:(n - 1)] = A \ rhs
    end

    a = (b_pontos[2:n] - b_pontos[1:(n - 1)]) ./ (3 .* h)
    b = b_pontos[1:(n - 1)]
    c = (y[2:n] - y[1:(n - 1)]) ./ h .- (h ./ 3) .* (2 .* b_pontos[1:(n - 1)] .+ b_pontos[2:n])
    d = y[1:(n - 1)]

    return a, b, c, d
end

function avalia_spline(
    xk :: Vector{<:Real},
    a :: Vector{<:Real},
    b :: Vector{<:Real},
    c :: Vector{<:Real},
    d :: Vector{<:Real},
    x :: Real
)
    n = length(xk)
    n >= 2 || throw(ArgumentError("xk deve conter pelo menos dois pontos"))
    m = n - 1
    all(length(v) == m for v in (a, b, c, d)) ||
        throw(DimensionMismatch("a, b, c e d devem ter comprimento length(xk) - 1"))

    for i in 1:(n - 1)
        xk[i] < xk[i + 1] || throw(ArgumentError("os valores de xk devem estar em ordem crescente e ser distintos"))
    end

    xk[1] <= x <= xk[end] || throw(DomainError(x, "x deve pertencer ao intervalo definido por xk"))

    i = clamp(searchsortedlast(xk, x), 1, m)
    dx = x - xk[i]
    return a[i] * dx^3 + b[i] * dx^2 + c[i] * dx + d[i]
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
            println("Entrada invalida. Use numeros separados por virgula, por exemplo: 0, 1, 3.")
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
            return [0.0, 1.0, 3.0], [1.0, 2.0, 0.0], [0.5, 2.0], true
        elseif opcao == "2"
            xk = ler_vetor("xk em ordem crescente")
            yk = ler_vetor("yk")
            xs = ler_vetor("pontos x para avaliar")
            return xk, yk, xs, false
        else
            println("Opcao invalida. Escolha 1 ou 2.")
        end
    end
end

# Painel interativo: use o exemplo ou informe seus proprios pontos.
xk, yk, xs, usar_exemplo = escolher_dados()
a, b, c, d = spline_natural(xk, yk)

println("\nCoeficientes do spline:")
println("a = ", a)
println("b = ", b)
println("c = ", c)
println("d = ", d)

for x in xs
    valor = avalia_spline(xk, a, b, c, d, x)
    println("S(", x, ") = ", valor)
    if usar_exemplo && x == 0.5
        @assert isapprox(valor, 1.625; atol=1e-12)
    elseif usar_exemplo && x == 2.0
        @assert isapprox(valor, 1.5; atol=1e-12)
    end
end

if usar_exemplo
    println("Teste do exemplo aprovado.")
end
