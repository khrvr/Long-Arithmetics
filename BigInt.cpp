#include <iostream>
#include <vector>
#include <string>

class BigInteger
{

public:

	static const long long base = 10000;
	static const int baseSize = 4;

private:

	bool positive = 1;
	std::vector <int> data = { 0 };

	void removeZeros() {
		size_t sz = data.size();
		while (sz > 1 && data[sz - 1] == 0) {
			--sz;
		}
		if (sz != data.size()) {
			data.resize(sz);
		}
		if (data.size() == 1 && data[0] == 0) {
			positive = 1;
		}
	}

	BigInteger cut(int amount) {
		std::vector <int> newVec(data.end() - amount, data.end());
		BigInteger newBI(0);
		newBI.data = newVec;
		data.resize(data.size() - amount);
		return newBI;
	}

	void append(BigInteger& other) {
		std::copy(other.data.begin(), other.data.end(), std::back_inserter(data));
	}

	void posPosPlusEqual(const BigInteger& other) {
		if (other.data.size() > data.size()) {
			data.resize(other.data.size(), 0);
		}
		bool leftOvers = 0;
		for (size_t i = 0; i < other.data.size(); ++i) {
			bool newLeftOvers = (leftOvers + data[i] + other.data[i] > base - 1);
			data[i] = (leftOvers + data[i] + other.data[i]) % base;
			leftOvers = newLeftOvers;
		}
		if (leftOvers) {
			if (data.size() <= other.data.size()) {
				data.push_back(leftOvers);
			}
			else {
				data[other.data.size()] += leftOvers;
			}
		}
		removeZeros();
	}
	
	void posNegPlusEqual(const BigInteger& bigger, const BigInteger& smaller) {
		bool leftOvers = 0;
		for (size_t i = 0; i < smaller.data.size(); ++i) {
			data[i] = bigger.data[i] - smaller.data[i] - leftOvers;
			if (data[i] < 0) {
				data[i] += base;
				leftOvers = 1;
			}
			else {
				leftOvers = 0;
			}
		}
		size_t smSize = smaller.data.size();
		data.resize(bigger.data.size(), 0);
		for (size_t i = smSize; i < bigger.data.size(); ++i) {
			data[i] = bigger.data[i] - leftOvers;
			if (data[i] < 0) {
				data[i] += base;
				leftOvers = 1;
			}
			else {
				leftOvers = 0;
			}
		}
		removeZeros();
	}

public:

	BigInteger(const BigInteger& s) : positive(s.positive), /*zero(s.zero),*/ data(s.data) {}

	BigInteger(int intint) : data(10 / baseSize + 3, 0) {
		if (intint) {
			if (intint < 0) {
				positive = 0;
				intint *= -1;
			}
			int sz = 0;
			while (intint) {
				data[sz] = intint % base;
				++sz;
				intint /= base;
			}
			data.resize(sz);
		}
		else {
			data.resize(1);
		}
	}

	BigInteger() {}

	explicit operator bool() const {
		return !isZero();
	}

	int sz() {
		return data.size();
	}

	void clear() {
		data.clear();
		positive = 1;
	}

	void push(int c) {
		data.push_back(c);
	}

	void changeSign() {
		positive = !positive;
	}

	void newSign(bool pos) {
		positive = pos;
	}

	bool isZero() const {
		if (data.size() == 1 && data[0] == 0) {
			return 1;
		}
		return 0;
	}

	bool pos() const {
		return positive;
	}

	BigInteger& operator-=(const BigInteger& other) {
		positive = !positive;
		operator+=(other);
		positive = !positive;
		return *this;
	}

	BigInteger& operator+=(const BigInteger& other) {
		if (positive == other.positive) {
			posPosPlusEqual(other);
		}
		else {
			if (isLessOrEqualWithoutSign(other)) {
				posNegPlusEqual(other, *this);
				positive = !positive;
			}
			else {
				posNegPlusEqual(*this, other);
			}
		}
		return *this;
	}

	BigInteger& operator*=(const BigInteger& other) {
		BigInteger sum(0);
		bool sizeCheck = (data.size() >= other.data.size());
		const BigInteger& localL = (sizeCheck ? *this : other);
		const BigInteger& localR = (sizeCheck ? other : *this);
		for (size_t i = 0; i < localR.data.size(); ++i) {
			if (localR.data[i] == 0) {
				continue;
			}
			BigInteger mult;
			mult.data.resize(i + localL.data.size(), 0);
			long long leftOvers = 0;
			for (size_t j = 0; j < localL.data.size(); ++j) {
				long long add = static_cast<long long>(localL.data[j]) * static_cast<long long>(localR.data[i]) + leftOvers;
				mult.data[i + j] = static_cast<int>(add % base);
				leftOvers = add / base;
			}
			if (leftOvers) {
				mult.data.push_back(leftOvers);
			}
			sum += mult;
		}
		sum.positive = (positive == other.positive);
		*this = sum;
		return *this;
	}

	BigInteger& operator/=(const BigInteger& other) {
		if (other.isZero()) {
			std::cerr << "Division by 0\n";
			return *this;
		}
		BigInteger ans(0);
		ans.data.pop_back();
		if (positive != other.positive) {
			ans.positive = 0;
		}
		positive = 1;
		BigInteger current(0);
		bool middle = 0;
		while (data.size() > other.data.size()) {
			bool extra = 0;
			for (size_t i = 0; i < other.data.size(); ++i) {
				if (data[data.size() - 1 - i] > other.data[other.data.size() - 1 - i]) {
					break;
				}
				else if (data[data.size() - 1 - i] < other.data[other.data.size() - 1 - i]) {
					extra = 1;
					break;
				}
			}
			size_t currentSize = (current.isZero() ? 0 : current.data.size());
			if (middle && (other.data.size() > currentSize)) {
				ans.data.insert(ans.data.begin(), (other.data.size() + extra - 1 - currentSize), 0);
			}
			current = cut(other.data.size() + static_cast<int>(extra));
			int count = binsearch(current, other, base);
			ans.data.insert(ans.data.begin(), count);
			if (!current.isZero()) {
				append(current);
			}
			else {
				for (int i = data.size() - 1; i >= 0; --i) {
					if (data[i] == 0) {
						data.pop_back();
						ans.data.insert(ans.data.begin(), 0);
					}
					else {
						break;
					}
				}
			}
			middle = 1;
		}
		if (other.isLessOrEqualWithoutSign(*this)) {
			ans.data.insert(ans.data.begin(), binsearch(*this, other, base));
		}
		else {
			int zeroCount = data.size() - (!current.isZero() ? current.data.size() : 0);
			if (zeroCount) {
				ans.data.insert(ans.data.begin(), zeroCount, 0);
			}
		}

		*this = ans;
		removeZeros();
		return *this;
	}

	BigInteger& operator++ () {
		return *this += 1;
	}

	BigInteger operator++ (int) {
		return *this += 1;
	}

	BigInteger& operator-- () {
		return *this -= 1;
	}

	BigInteger operator-- (int) {
		return *this -= 1;
	}

	BigInteger operator- () const {
		BigInteger ss = *this;
		if (ss) {
			ss.positive = !ss.positive;
		}
		return ss;
	}

	std::string toString() const {
		std::string str = "";
		str.reserve(baseSize * (data.size() + 2));
		if (!positive) {
			str += '-';
		}
		str += std::to_string(data[data.size() - 1]);
		size_t sz = str.size();
		str.resize(baseSize * (data.size() - 1) + sz);
		for (int i = data.size() - 2; i >= 0; --i) {
			int tmp = data[i];
			for (size_t j = 0; j < baseSize; ++j) {
				str[sz + baseSize - j - 1] = static_cast<char>('0' + (tmp % 10));
				tmp /= 10;
			}
			sz += baseSize;
		}
		return str;
	}

	std::string toString(size_t precision) const {
		std::string str = toString();
		if (str[0] == '-') {
			str.erase(0, 1);
		}
		if (precision >= str.size()) {
			str.insert(0, precision + 1 - str.size(), '0');
		}
		str.insert(str.size() - precision, 1, '.');
		if (!positive) {
			str.insert(0, "-");
		}
		return str;
	}

	bool isLessWithoutSign(const BigInteger& r) const {
		if (data.size() < r.data.size()) {
			return 1;
		}
		if (data.size() > r.data.size()) {
			return 0;
		}
		for (int i = data.size() - 1; i >= 0; --i) {
			if (data[i] < r.data[i]) {
				return 1;
			}
			if (data[i] > r.data[i]) {
				return 0;
			}
		}
		return 0;
	}

	bool isLessOrEqualWithoutSign(const BigInteger& r) const {
		if (data.size() < r.data.size()) {
			return 1;
		}
		if (data.size() > r.data.size()) {
			return 0;
		}
		for (int i = data.size() - 1; i >= 0; --i) {
			if (data[i] < r.data[i]) {
				return 1;
			}
			if (data[i] > r.data[i]) {
				return 0;
			}
		}
		return 1;
	}

	bool isEqualWithoutSign(const BigInteger& r) const {
		if (data.size() != r.data.size()) {
			return 0;
		}
		for (size_t i = 0; i < data.size(); ++i) {
			if (data[i] != r.data[i]) {
				return 0;
			}
		}
		return 1;
	}

	BigInteger actualPow(size_t precision) const {
		BigInteger ans = 1;
		ans.data.insert(ans.data.begin(), precision / baseSize, 0);
		for (size_t i = 0; i < precision % baseSize; ++i) {
			ans.data[ans.data.size() - 1] *= 10;
		}
		ans *= *this;
		return ans;
	}

	friend int binsearch(BigInteger& current, const BigInteger& r, int bsr);
};

BigInteger operator+(const BigInteger& l, const BigInteger& r) {
	BigInteger tmp = l;
	return tmp += r;
}

BigInteger operator-(const BigInteger& l, const BigInteger& r) {
	BigInteger tmp = l;
	return  tmp -= r;
}

BigInteger operator*(const BigInteger& l, const BigInteger& r) {
	BigInteger tmp = l;
	return tmp *= r;
}

BigInteger operator/(const BigInteger& l, const BigInteger& r) {
	BigInteger tmp = l;
	return tmp /= r;
}

BigInteger operator%(const BigInteger& l, const BigInteger& r) {
	return  l - (l / r) * r;
}

BigInteger& operator%=(BigInteger& first, const BigInteger& other) {
	first = first % other;
	return first;
}

bool operator==(const BigInteger& l, const BigInteger& r) {
	if (l.isZero() && r.isZero()) {
		return 1;
	}
	if (l.pos() != r.pos()) {
		return 0;
	}
	return l.isEqualWithoutSign(r);
}

bool operator!= (const BigInteger& l, const BigInteger& r) {
	return !(l == r);
}

bool operator<(const BigInteger& l, const BigInteger& r) {
	if (l.isZero() && r.isZero()) {
		return 0;
	}
	if (l.pos()) {
		if (r.pos()) {
			return l.isLessWithoutSign(r);
		}
		return 0;
	}
	if (r.pos()) {
		return 1;
	}
	return r.isLessWithoutSign(l);
}

bool operator<=(const BigInteger& l, const BigInteger& r) {
	if (l.isZero() && r.isZero()) {
		return 1;
	}
	if (l.pos()) {
		if (r.pos()) {
			return l.isLessOrEqualWithoutSign(r);
		}
		return 0;
	}
	if (r.pos()) {
		return 1;
	}
	return r.isLessOrEqualWithoutSign(l);
}

bool operator>(const BigInteger& l, const BigInteger& r) {
	return !(l <= r);
}

bool operator>=(const BigInteger& l, const BigInteger& r) {
	return !(l < r);
}

std::istream& operator>> (std::istream& cin, BigInteger& s) {
	s.clear();
	std::string input;
	cin >> input;
	if (input[0] == '-' || input[0] == '+') {
		if (input[0] == '-') {
			s.changeSign();
		}
		input.erase(input.begin());
	}
	size_t eraseCount = 0;
	while (eraseCount < input.size() - 1 && input[eraseCount] == '0') {
		++eraseCount;
	}
	input.erase(0, eraseCount);
	for (int i = input.size(); i - s.baseSize >= 0; i -= s.baseSize) {
		s.push(std::stoi(input.substr(i - s.baseSize, s.baseSize)));
	}
	if (input.size() % s.baseSize != 0) {
		s.push(std::stoi(input.substr(0, input.size() % s.baseSize)));
	}
	return cin;
}

std::ostream& operator<< (std::ostream& cout, const BigInteger& s) {
	cout << s.toString();
	return cout;
}

int binsearch(BigInteger& current, const BigInteger& r, int bsr) {
	int bsl = 0;
	int prevbsm = 0;
	int sign = 2 * r.pos() - 1;
	while (bsr > bsl) {
		int bsm = (bsr + bsl) / 2;
		BigInteger tmp((sign * (prevbsm - bsm)));
		bool pos1 = (current += tmp * r).pos();
		bool pos2 = r.isLessOrEqualWithoutSign(current);
		if (current.isZero()) {
			return bsm;
		}
		else if (pos1 && pos2) {
			prevbsm = bsm;
			bsl = bsm;
		}
		else if (!pos1) {
			prevbsm = bsm;
			bsr = bsm;
		}
		else if (pos1 && !pos2) {
			return bsm;
		}
	}
	return 0;
}

BigInteger gcd(BigInteger& f, BigInteger& s) {
	if (s == 0) {
		return f;
	}
	return gcd(s, f %= s);
}

void gcdDivision(BigInteger& first, BigInteger& second) {
	BigInteger f = first;
	f.newSign(1);
	BigInteger s = second;
	s.newSign(1);
	BigInteger gcdgcd = gcd(f, s);
	first /= gcdgcd;
	second /= gcdgcd;
}

class Rational
{
	BigInteger num = 0;
	BigInteger denom = 1;

public:

	Rational(const BigInteger& bi) : num(bi) {}

	Rational(int i) : num(i) {}

	Rational() {}

	Rational(const BigInteger& bi, int i) : num(bi), denom(i) {
		gcdDivision(num, denom);
	}

	Rational& operator=(const Rational& other) {
		num = other.num;
		denom = other.denom;
		return *this;
	}

	BigInteger& numerator() {
		return num;
	}

	BigInteger& denominator() {
		return denom;
	}

	BigInteger numerator() const {
		return num;
	}

	BigInteger denominator() const {
		return denom;
	}

	Rational operator-() const {
		Rational r = *this;
		if (num) {
			r.num.changeSign();
		}
		return r;
	}

	Rational& operator+=(const Rational& other) {
		num = num * other.denom + denom * other.num;
		denom *= other.denom;
		gcdDivision(num, denom);
		return *this;
	}

	Rational& operator-=(const Rational& other) {
		num = num * other.denom - denom * other.num;
		denom *= other.denom;
		gcdDivision(num, denom);
		return *this;
	}

	Rational& operator*=(const Rational& other) {
		num *= other.num;
		denom *= other.denom;
		gcdDivision(num, denom);
		return *this;
	}

	Rational& operator/=(const Rational& other) {
		BigInteger tmp = other.num;
		num *= other.denom;
		if (!other.num.pos()) {
			tmp.changeSign();
			num.changeSign();
		}
		denom *= tmp;
		if (denom.isZero()) {
			std::cerr << "Division by 0 in Rational\n";
		}
		gcdDivision(num, denom);
		return *this;
	}

	std::string toString() const {
		return num.toString() + (denom > 1 ? "/" + denom.toString() : "");
	}

	std::string asDecimal(size_t precision) const {
		if (precision) {
			return ((num.actualPow(precision)) / denom).toString(precision);
		}
		else {
			return (num / denom).toString();
		}
	}

	explicit operator double() const {
		return stod(asDecimal(18));
	}
};

bool operator==(const Rational& l, const Rational& r) {
	return ((l.numerator() == r.numerator() && l.denominator() == r.denominator()));
}

bool operator!=(const Rational& l, const Rational& r) {
	return !(l == r);
}

bool operator<=(const Rational& l, const Rational& r) {
	return (l.numerator() * r.denominator()) <= (l.denominator() * r.numerator());
}

bool operator>=(const Rational& l, const Rational& r) {
	return (l.numerator() * r.denominator()) >= (l.denominator() * r.numerator());
}

bool operator<(const Rational& l, const Rational& r) {
	return (l.numerator() * r.denominator()) < (l.denominator() * r.numerator());
}

bool operator>(const Rational& l, const Rational& r) {
	return (l.numerator() * r.denominator()) > (l.denominator() * r.numerator());
}

Rational operator+(const Rational& l, const Rational& r) {
	Rational newL(l);
	return newL += r;
}

Rational operator-(const Rational& l, const Rational& r) {
	Rational newL(l);
	return newL -= r;
}

Rational operator*(const Rational& l, const Rational& r) {
	Rational newL(l);
	return newL *= r;
}

Rational operator/(const Rational& l, const Rational& r) {
	Rational newL(l);
	return newL /= r;
}
