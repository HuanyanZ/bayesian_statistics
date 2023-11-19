import numpy as np
import matplotlib.pyplot as plt


class GP:
    def __init__(self, x, y, s=0.0001):
        self.x = x
        self.y = y
        self.s = s
        self.n = len(x)

    def mean(self):
        return [0] * self.n

    def kernel(self, a, b):
        sigma_f = 1
        l = 0.1
        return (sigma_f ^ 2) * np.exp(-0.5 * np.square(np.subtract.outer(a, b)) / l)

    def prior(self, k=0):
        if k == 0:
            k = self.n
        y = np.random.multivariate_normal(self.mean(), self.cov(), check_valid="raise")
        return y

    def predictor(self, input):
        L = np.linalg.cholesky(self.kernel(self.x, self.x) + self.s * np.eye(self.n))
        v = np.linalg.solve(L, self.kernel(self.x, input))

        mean = np.dot(
            np.linalg.solve(L, self.kernel(self.x, input)).T, np.linalg.solve(L, self.y)
        )
        covariance = self.kernel(input, input) - np.dot(v.T, v)
        return mean, covariance

    def sample(self, input):
        f_star_mean, f_star_cov = self.predictor(input)

        cov_epsilon = np.eye(len(input))

        return np.random.multivariate_normal(
            f_star_mean, f_star_cov + cov_epsilon, check_valid="raise"
        )


if __name__ == "__main__":
    # np.random.seed(9937)

    s = 0.00005
    n = 20

    x = np.sort(np.random.uniform(-5, 5, n))
    f = lambda x: np.sin(0.9 * x).flatten()
    y = f(x) + s * np.random.randn(n)

    plt.plot(x, y, "k+")

    gp = GP(x, y, s)

    m = 200
    # x_star = np.sort(np.random.uniform(-5, 5, m))
    # x_star = np.linspace(-5, 5, m)
    x_star = x

    plt.plot(x_star, f(x_star), "b-")

    mean, cov = gp.predictor(x_star)
    # y_star = gp.sample(x_star)

    plt.plot(x_star, mean, "r--")

    sd = np.sqrt(np.diag(cov))
    print(sd)

    plt.gca().fill_between(x_star, mean - 2 * sd, mean + 2 * sd, color="#dddddd")

    plt.show()
